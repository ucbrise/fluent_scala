package fluent

import scala.collection.mutable

trait Collection[A <: Product] {
  val name: String
  val columns: List[String]
  def get(): mutable.Set[A]
  def merge(t: A)
  def delete(t: A)
  def tick()
}
