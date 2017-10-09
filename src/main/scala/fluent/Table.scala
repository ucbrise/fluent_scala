package fluent

import scala.collection.mutable

class Table[A <: Product](val name: String, val columns: List[String]) extends Collection[A] {
  private var tuples: mutable.Set[A] = mutable.Set()

  override def get(): mutable.Set[A] = {
    tuples
  }

  override def merge(t: A) = {
    tuples += t
  }

  override def delete(t: A) = {
    tuples -= t
  }

  override def tick() = {
  }
}
