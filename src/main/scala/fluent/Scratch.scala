package fluent

import scala.collection.mutable

class Scratch[A](val name: String) extends Collection[A] {
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
    tuples.clear()
  }
}
