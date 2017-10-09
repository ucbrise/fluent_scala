package fluent

import scala.collection.mutable
import scala.collection.immutable

import akka.actor.{ Actor, ActorLogging, ActorRef, ActorSystem, Props }
import com.typesafe.config.ConfigFactory

object FluentActor {
  def props(): Props = {
    Props[FluentActor]
  }

  case class Message(name: String, t: Product)
}

class FluentActor(
    collections: immutable.Map[String, Collection[Product]],
    bootstrap_rules: List[Rule[Product]],
    rules: List[Rule[Product]])
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

  private def eval_rule(rule: Rule[Product]) = {
    rule match {
      case Rule(c: Channel[Product], Merge(), ra) => {
        for (t <- RelAlg.eval(ra)) {
          val dst_hostport = t.productElement(0)
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

  def erased_brules = bootstrap_rules.map({case (r: Rule[Product]) => r})
  def erased_rules = rules.map({case (r: Rule[Product]) => r})
  def erased_collections = collections.map({case (c: Collection[Product]) => c})

  def hostport(): String = {
    s"$host:$port"
  }

  def isMonotonic(): Boolean = {
    val isRelalgOk = (ra: RelAlg[Product]) => {
      ra match {
        case (_: Diff[_]) | (_: Group[_, _, _]) => false
        case _ => true
      }
    }
    val isRuleOk = (rule: Rule[Product]) => {
      rule match {
        case Rule(_, Delete(), _) => false
        case Rule(_, _, ra) => isRelalgOk(ra)
      }
    }
    erased_brules.forall(isRuleOk) && erased_rules.forall(isRuleOk)
  }

  def run(): (ActorSystem, ActorRef) = {
    val hostport = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(hostport).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val collection_map = erased_collections.map(c => (c.name, c)).toMap
    val actor = system.actorOf(Props(new FluentActor(collection_map, erased_brules, erased_rules)), "fluent")
    (system, actor)
  }
}
