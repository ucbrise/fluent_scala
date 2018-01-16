package fluent

import scala.language.reflectiveCalls

import Embedding.Predef._
import Embedding.Quasicodes._
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import com.typesafe.config.ConfigFactory
import squid.utils._

object FluentActor {
  def props(channels: Map[String, Channel.Existential],
            rules: () => Unit)
            : Props = {
    Props(new FluentActor(channels, rules))
  }

  case class Message(name: String, t: Any)
}

class FluentActor(
    channels: Map[String, Channel.Existential],
    rules: () => Unit)
    extends Actor {
  override def receive = {
    case FluentActor.Message(name, t) => {
      channels(name) match {
        case c: Channel[a] => c.addEqual(Set(t.asInstanceOf[a]))
      }
      rules()
      channels.foreach({case (_, c) => c.clear()})
    }
  }
}

trait FluentProgram {
  // Abstract //////////////////////////////////////////////////////////////////
  val name: String
  val host: String
  val port: Int
  val channels: Map[String, Channel.Existential]

  type Context
  val rules: IR[Unit, Context]
  val assignment: Assignment[Context]

  // Provided //////////////////////////////////////////////////////////////////
  def hostport(): String = s"$host:$port"

  def rulesList(): List[IR[Unit, _]] = {
    def rulesListImpl(rules: IR[Unit, _]): List[IR[Unit, _]] = {
      rules match {
        case ir"($x: Unit);$xs" => List(x) ++ rulesListImpl(xs)
      }
    }
    rulesListImpl(rules)
  }

  def isValidRule(rule: IR[Unit, _]): Boolean = {
    rule match {
      // BoolOrLattice
      case ir"($head: BoolOrLattice) <-- $body" => true
      case ir"($head: BoolOrLattice) &&= $body" => true
      case ir"($head: BoolOrLattice) ||= $body" => true
      // IntMaxLattice
      case ir"($head: IntMaxLattice) <-- $body" => true
      case ir"($head: IntMaxLattice) <== $body" => true
      case ir"($head: IntMaxLattice) += $body" => true
      case ir"($head: IntMaxLattice) -= $body" => true
      // SetUnionLattice[_]
      case ir"($head: SetUnionLattice[$t]) <-- $body" => true
      case ir"($head: SetUnionLattice[$t]) += $body" => true
      case ir"($head: SetUnionLattice[$t]) -= $body" => true
      // Otherwise.
      case _ => false
    }
  }

  def isMonotonicRule(rule: IR[Unit, _]): Boolean = {
    def isMonotonicExpr[A: IRType](e: IR[A, _]): Boolean = {
      e match {
        // BoolOrLattice
        case ir"($lhs: BoolOrLattice) merge ($rhs: BoolOrLattice)" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: BoolOrLattice) && ($rhs: BoolOrLattice)" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: BoolOrLattice) || ($rhs: BoolOrLattice)" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: IntMaxLattice) > ($_: Int)" => isMonotonicExpr(lhs)
        case ir"($lhs: IntMaxLattice) >= ($_: Int)" => isMonotonicExpr(lhs)
        case ir"!($e: BoolOrLattice)" => false

        // IntMaxLattice
        case ir"($lhs: IntMaxLattice) merge ($rhs: IntMaxLattice)" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: IntMaxLattice) + ($rhs: IntMaxLattice)" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: IntMaxLattice) - ($rhs: IntMaxLattice)" => false
        case ir"($e: SetUnionLattice[$t]).size()" => isMonotonicExpr(e)

        // SetUnionLattice
        case ir"($lhs: SetUnionLattice[$t]) merge ($rhs: SetUnionLattice[t])" => {
          isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        }
        case ir"($lhs: SetUnionLattice[$t]).filter($_)" => isMonotonicExpr(lhs)
        case ir"($lhs: SetUnionLattice[$t]).map($_)($bev)" => isMonotonicExpr(lhs)
        //case ir"($lhs: SetUnionLattice[$t]) cross ($lhs: SetUnionLattice[$u])" => {
          //isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        //}
        //case ir"($lhs: SetUnionLattice[$t]) union ($lhs: SetUnionLattice[t])" => {
          //isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        //}
        //case ir"($lhs: SetUnionLattice[$t]) intersect ($lhs: SetUnionLattice[t])" => {
          //isMonotonicExpr(lhs) && isMonotonicExpr(rhs)
        //}
        //case ir"($lhs: SetUnionLattice[$t]) diff ($lhs: SetUnionLattice[t])" => false
      }
    }

    rule match {
      case ir"($head: BoolOrLattice) ||= $body" => true
      case ir"($head: IntMaxLattice) <== $body" => true
      case ir"($head: IntMaxLattice) += $body" => true
      case ir"($head: SetUnionLattice[$t]) += $body" => true
      case _ => false
    }
  }

  def isIncreasingRule(rule: IR[Unit, _]): Boolean = {
    rule match {
      case ir"($head: BoolOrLattice) ||= $body" => true
      case ir"($head: IntMaxLattice) <== $body" => true
      case ir"($head: IntMaxLattice) += $body" => true
      case ir"($head: SetUnionLattice[$t]) += $body" => true
      case _ => false
    }
  }

  def isValid(): Boolean = {
    rulesList().forall(isValidRule)
  }

  def isMonotonic(): Boolean = {
    rulesList().forall(r => isIncreasingRule(r) && isMonotonicRule(r))
  }

  def isIncreasing(): Boolean = {
    rulesList().forall(isIncreasingRule)
  }

  def run(): (ActorSystem, ActorRef) = {
    val addr = s"akka.remote.netty.tcp.port=$port"
    val fallback = ConfigFactory.load()
    val config = ConfigFactory.parseString(addr).withFallback(fallback)
    val system = ActorSystem("fluent", config)

    val rules_f = assignment.compile(ir"() => $rules")
    val props = FluentActor.props(channels, rules_f)
    val actor = system.actorOf(props, "fluent")
    (system, actor)
  }
}
