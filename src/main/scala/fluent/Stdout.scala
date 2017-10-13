package fluent

import scala.collection.mutable

class Stdout extends Collection[Tuple1[String]] {
  override val name = "stdout"

  override def get(): mutable.Set[Tuple1[String]] = {
    throw new UnsupportedOperationException("Stdout does not support get.")
  }

  override def merge(t: Tuple1[String]) = {
    println(t._1)
  }

  override def delete(t: Tuple1[String]) = {
    throw new UnsupportedOperationException("Stdout does not support deletion.")
  }

  override def tick() = {
  }
}
