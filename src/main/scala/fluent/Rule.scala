package fluent

sealed trait RuleType
case class Merge() extends RuleType
case class Delete() extends RuleType

case class Rule[A](c: Collection[A], ruleType: RuleType, ra: RelAlg[A])

object Rule {
  implicit class Infix[A](c: Collection[A]) {
    def +=(ra: RelAlg[A]): Rule[A] = {
      Rule(c, Merge(), ra)
    }

    def -=(ra: RelAlg[A]): Rule[A] = {
      Rule(c, Delete(), ra)
    }
  }
}
