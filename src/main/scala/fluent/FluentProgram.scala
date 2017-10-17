package fluent

import scala.language.reflectiveCalls

import Embedding.Predef._
import BoolOrLattice.myvar

class FluentActor(vars: Map[String, BoolOrLattice], bootstrap_rules: List[Code[Unit]]) {
  if (bootstrap_rules.length != 0) {
    bootstrap_rules.foreach(eval_rule)
  }

  private def eval_rule(rule: Code[Unit]) = {
    val rule2 = rule rewrite {
      case ir"myvar[BoolOrLattice](${name})" => ir"(?varsRuntime:Map[String,BoolOrLattice])($name)"
    }
    val ruleFun = ir"(varsRuntime: Map[String, BoolOrLattice]) => $rule2"
    println(rule)
    println(rule2)
    ruleFun.asClosedIR.run.apply(vars)
  }
}

trait FluentProgram {
  val name: String
  val host: String
  val port: Int
  val bootstrap_rules: List[Code[Unit]] = List()
  val vars: Map[String, BoolOrLattice]

  //def hostport(): String = s"$host:$port"
  //def isMonotonic(): Boolean = rules.forall(_.isMonotonic())
  //def isIncreasing(): Boolean = rules.forall(_.isIncreasing())

  def run() = {
    //val addr = s"akka.remote.netty.tcp.port=$port"
    //val fallback = ConfigFactory.load()
    //val config = ConfigFactory.parseString(addr).withFallback(fallback)
    //val system = ActorSystem("fluent", config)

    //val channels: Map[String, Channel.Existential] =
      //rules.flatMap(_.channels()).map(c => (c.name, c)).toMap
    val props = new FluentActor(vars, bootstrap_rules)

    //val actor = system.actorOf(props, "fluent")
    //(system, actor)
  }
}
