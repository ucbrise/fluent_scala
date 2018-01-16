package examples.lattices

import fluent.Embedding.Predef._
import fluent.Embedding.Quasicodes._
import fluent._

class Example extends FluentProgram {
  val t = new SetUnionLattice[Int]()
  val x = new IntMaxLattice(0)
  val b = new BoolOrLattice(false)

  override val name = "lattices_example"
  override val host = "localhost"
  override val port = 8000
  override val channels = Map[String, Channel.Existential]()

  override type Context = t.Ctx with x.Ctx with b.Ctx
  override val rules = ir{
    ${t} += ${t}
    ${x} += ${t}.size() - ${x}
    ${b} &&= ${t}.size() > 10
  }
  override val assignment = t & x & b
}

object Example extends App {
  val example = new Example()
  println(s"This program is monotonic? ${example.isMonotonic()}")
  println(s"This program is increasing? ${example.isIncreasing()}")
}
