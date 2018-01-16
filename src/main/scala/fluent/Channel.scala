package fluent

import scala.language.implicitConversions

case class Channel[A <: {val dst: String}](val name: String) {
  private var xs: Set[A] = Set()

  def get() = xs
  def clear() = {xs = Set()}

  def assign(that: SetUnionLattice[A]) = {xs = that.xs}
  def addEqual(xs: Set[A]) = {this.xs = this.xs.union(xs)}
  def subtractEqual(that: SetUnionLattice[A]) = {xs = xs.diff(that.xs)}
}

object Channel {
  type Existential = Channel[_ <: {val dst: String}];
}
