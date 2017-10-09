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
    rules: List[Rule[Product]])
  extends Actor {
  import FluentActor._

  override def receive = {
    case Message(name, t) => {
      collections(name).merge(t)
      step()
    }
  }

  private def step() = {
    for (rule <- rules) {
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

    for (collection <- collections.values) {
      collection.tick()
    }
  }
}

trait FluentProgram {
  val name: String
  val host: String
  val port: Int
  val collections: List[Any]
  val rules: List[Any]

  def run(): (ActorSystem, ActorRef) = {
    val hostport = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(hostport).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val erased_rules = rules.map({case (rule: Rule[Product]) => rule})
    val erased_collections = collections.map({case (c: Collection[Product]) => c})
    val collection_map = erased_collections.map(c => (c.name, c)).toMap
    val actor = system.actorOf(Props(new FluentActor(collection_map, erased_rules)), name)
    (system, actor)
  }
}
