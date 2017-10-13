package fluent

import scala.collection.mutable
import scala.collection.immutable

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object FluentActor {
  def props(collections: immutable.Map[String, Collection[Any]],
            bootstrap_rules: List[Rule[Any]],
            rules: List[Rule[Any]]): Props = {
    Props(new FluentActor(collections, bootstrap_rules, rules))
  }

  case class Message(name: String, t: Any)
}

class FluentActor(
    collections: immutable.Map[String, Collection[Any]],
    bootstrap_rules: List[Rule[Any]],
    rules: List[Rule[Any]])
  extends Actor {
  import FluentActor._

  if (bootstrap_rules.length != 0) {
    bootstrap_rules.foreach(eval_rule)
    collections.values.foreach(_.tick())
  }

  override def receive = {
    case Message(name, t) => {
      collections(name).merge(t)
      step()
    }
  }

  private def eval_rule(rule: Rule[Any]) = {
    rule match {
      case Rule(c: Channel[_], Merge(), ra) => {
        for (t <- RelAlg.eval(ra)) {
          //val dst_hostport = t.productElement(0)
          val dst_hostport = t.asInstanceOf[AnyRef{val dst:String}].dst
          val dst_addr = s"akka.tcp://fluent@${dst_hostport}/user/fluent"
          val msg = Message(c.name, t)
          context.actorSelection(dst_addr) ! msg
        }
      }
      case Rule(c, ruleType, ra) => {
        for (t <- RelAlg.eval(ra)) {
          ruleType match {
            case Merge() => c.merge(t)
            case Delete() => c.delete(t)
          }
        }
      }
    }
  }

  private def step() = {
    rules.foreach(eval_rule)
    collections.values.foreach(_.tick())
  }
}

trait FluentProgram {
  val name: String
  val host: String
  val port: Int
  val collections: List[Any]
  val bootstrap_rules: List[Any] = List()
  val rules: List[Any]

  private def erased_collections: List[Collection[Any]] = {
    collections.map({case (c: Collection[Any]) => c})
  }

  private def erased_bootstrap_rules: List[Rule[Any]] = {
    bootstrap_rules.map({case (r: Rule[Any]) => r})
  }

  private def erased_rules: List[Rule[Any]] = {
    rules.map({case (r: Rule[Any]) => r})
  }

  def hostport(): String = {
    s"$host:$port"
  }

  def isMonotonic(): Boolean = {
    val isRelalgOk = (ra: RelAlg[Any]) => {
      ra match {
        case (_: Diff[_]) | (_: Group[_, _, _]) => false
        case _ => true
      }
    }
    val isRuleOk = (rule: Rule[Any]) => {
      rule match {
        case Rule(_, Delete(), _) => false
        case Rule(_, _, ra) => isRelalgOk(ra)
      }
    }
    erased_bootstrap_rules.forall(isRuleOk) && erased_rules.forall(isRuleOk)
  }

  def run(): (ActorSystem, ActorRef) = {
    val hostport = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(hostport).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val collection_map = erased_collections.map(c => (c.name, c)).toMap
    val props = FluentActor.props(
      collection_map,
      erased_bootstrap_rules,
      erased_rules)
    val actor = system.actorOf(props, "fluent")
    (system, actor)
  }
}
