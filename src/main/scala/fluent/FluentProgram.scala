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

  case class Message(name: String, t: AnyRef{val dst: String})
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

  def addToChannel[A <: AnyRef{val dst: String}](c: Channel[A], t: Any) = {
    c += Set(t.asInstanceOf[A])
  }

  override def receive = {
    case Message(name, t) => {
      addToChannel(channels(name), t)
      rules.foreach(eval_rule)
      channels.foreach({case (_, c) => c.clear()})
    }
  }

  private def eval_rule(rule: Rule) = {
    rule match {
      case SetUnionLatticeRule(r) => SetUnionLattice.Rule.eval(r)
      case IntMaxLatticeRule(r) => IntMaxLattice.Rule.eval(r)
      case BoolOrLatticeRule(r) => BoolOrLattice.Rule.eval(r)
      case StdOutRule(r) => StdOut.Rule.eval(r)
      case ChannelRule(r) => {
        val v = r.e.eval()
        r.m match {
          case Channel.AddEqual => {
            for (x <- v.xs) {
              val dst_hostport = x.dst
              val dst_addr = s"akka.tcp://fluent@${dst_hostport}/user/fluent"
              val msg = Message(r.l.name, x)
              context.actorSelection(dst_addr) ! msg
            }
          }
          case Channel.SubtractEqual => r.l -= v
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

  def hostport(): String = {
    s"$host:$port"
  }

  def isMonotonic(): Boolean = {
    rules.forall(Rule.isMonotonic(_))
  }

  def isIncreasing(): Boolean = {
    rules.forall(Rule.isIncreasing(_))
  }

  def run(): (ActorSystem, ActorRef) = {
    val addr = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(addr).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val props = FluentActor.props(channels(), bootstrap_rules, rules)
    val actor = system.actorOf(props, "fluent")
    (system, actor)
  }

  private def channels(): Map[String, Channel.Existential] = {
    rules.flatMap(Rule.channels(_)).map(c => (c.name, c)).toMap
  }
}
