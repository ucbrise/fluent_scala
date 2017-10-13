package fluent

import scala.collection.mutable

trait Collection[A] {
  val name: String
  def get(): mutable.Set[A]
  def merge(t: A)
  def delete(t: A)
  def tick()
}
