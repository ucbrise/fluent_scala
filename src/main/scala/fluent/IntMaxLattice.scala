package fluent

import scala.math
import scala.language.implicitConversions

import squid.utils._
import Embedding.Predef._

case class IntMaxLattice(private var x: Int)
  extends Embedding.SelfAssignedVariable[IntMaxLattice]
  with Lattice[IntMaxLattice]
{
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: IntMaxLattice): Option[Int] = {
    Some(x.compareTo(that.x))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: IntMaxLattice): IntMaxLattice = {
    new IntMaxLattice(math.max(x, that.x))
  }

  def +(that: IntMaxLattice): IntMaxLattice = IntMaxLattice(x + that.x)
  def -(that: IntMaxLattice): IntMaxLattice = IntMaxLattice(x - that.x)
  def >(x: Int): BoolOrLattice = BoolOrLattice(this.x > x)
  def >=(x: Int): BoolOrLattice = BoolOrLattice(this.x >= x)

  // Methods ///////////////////////////////////////////////////////////////////
  def <--(that: IntMaxLattice) = { x = that.x }
  def <==(that: IntMaxLattice) = { x = merge(that).x }
  def +=(that: IntMaxLattice) = { x += that.x }
  def -=(that: IntMaxLattice) = { x -= that.x }
}

