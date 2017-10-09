package fluent

class Example extends FluentProgram {
  val t = new Table[(Int, Int)]("t", List("x", "y"))
  val s = new Scratch[(String, String)]("s", List("x", "y"))

  override val rules = {
    import fluent.Rule.Infix
    List(
      t += Relation(t),
      s += Relation(s)
    )
  }
}
