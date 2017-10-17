package fluent

import scala.language.reflectiveCalls

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory

object FluentActor {
  def props(channels: Map[String, Channel.Existential],
            bootstrap_rules: List[Rule],
            rules: List[Rule])
            : Props = {
    Props(new FluentActor(channels, bootstrap_rules, rules))
  }

  case class Message(name: String, t: Any)
}

class FluentActor(
    channels: Map[String, Channel.Existential],
    bootstrap_rules: List[Rule],
    rules: List[Rule])
    extends Actor {
  import FluentActor._

  if (bootstrap_rules.length != 0) {
    bootstrap_rules.foreach(eval_rule)
  }

  override def receive = {
    case Message(name, t) => {
      channels(name) match {
        case c: Channel[a] => c.addEqual(Set(t.asInstanceOf[a]))
      }
      rules.foreach(eval_rule)
      channels.foreach({case (_, c) => c.clear()})
    }
  }

  private def eval_rule(rule: Rule) = {
    rule match {
      case SetUnionLatticeRule(r) => r.eval()
      case IntMaxLatticeRule(r) => r.eval()
      case BoolOrLatticeRule(r) => r.eval()
      case StdOutRule(r) => r.eval()
      case ChannelRule(r) => {
        val v = r.e.eval()
        r.m match {
          case Channel.SubtractEqual => r.l.subtractEqual(v)
          case Channel.AddEqual => {
            for (x <- v.xs) {
              val dst_addr = s"akka.tcp://fluent@${x.dst}/user/fluent"
              context.actorSelection(dst_addr) ! Message(r.l.name, x)
            }
          }
        }
      }
    }
  }
}

trait FluentProgram {
  val name: String
  val host: String
  val port: Int
  val bootstrap_rules: List[Rule] = List()
  val rules: List[Rule]

  def hostport(): String = s"$host:$port"
  def isMonotonic(): Boolean = rules.forall(_.isMonotonic())
  def isIncreasing(): Boolean = rules.forall(_.isIncreasing())

  def run(): (ActorSystem, ActorRef) = {
    val addr = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(addr).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val channels: Map[String, Channel.Existential] =
      rules.flatMap(_.channels()).map(c => (c.name, c)).toMap
    val props = FluentActor.props(channels, bootstrap_rules, rules)
    val actor = system.actorOf(props, "fluent")
    (system, actor)
  }
}
