package examples.lattices

import fluent._

import Embedding.Predef._
import Embedding.Quasicodes._
import BoolOrLattice.toCode

class Example extends FluentProgram {
  val b1 = new BoolOrLattice("b1", true)
  val b2 = new BoolOrLattice("b2", false)

  override val name = "lattices_example"
  override val host = "localhost"
  override val port = 8000
  override val vars = {
    Map("b1" -> b1, "b2" -> b2)
  }

  def toRuleList(block: IR[Any, Any]): List[IR[Unit, Any]] = {
    block match {
      //case ir"$r:Unit;$rest:Any" => r::toRuleList(rest)
      case ir"squid.lib.Imperative[Unit]($xs*)($r)" => xs.flatMap(toRuleList).toList ++ toRuleList(r)
      case ir"($r: BoolOrLattice) ||= $e" => List(block.asInstanceOf[IR[Unit, Any]])
    }
  }

  override val bootstrap_rules= {
    val xs = toRuleList(ir {
      ${b2} ||= ${b1}
      ${b2} &&= ${b1}
      ${b2} ||= ${b1}
    })
    println(s"Size = ${xs.size}")
    xs
    //List(
      //ir"$b2 ||= $b1"
      ////ir"${toCode(b2)} &&= (${toCode(b1)} || ${toCode(b2)})"
      ////ir"$b1 ||= $b2"
    //)
  }
}

object Main extends App {
  val example = new Example()
  println(example.b1)
  println(example.b2)
  example.run
  println(example.b1)
  println(example.b2)
  //println(s"This program is monotonic? ${Example.isMonotonic()}")
  //println(s"This program is increasing? ${Example.isIncreasing()}")
}
