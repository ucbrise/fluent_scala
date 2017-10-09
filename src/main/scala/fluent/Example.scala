package fluent

class Example extends FluentProgram {
  val t = new Table[(Int, Int)]("t", List("x", "y"))
  val s = new Scratch[(String, String)]("s", List("x", "y"))

  override val name = "example"
  override val host = "localhost"
  override val port = 8000

  override val collections = {
    List(t, s)
  }

  override val rules = {
    import fluent.Rule.Infix
    List(
      t += Relation(t),
      s += Relation(s)
    )
  }
}
