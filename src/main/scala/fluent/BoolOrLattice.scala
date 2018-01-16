package fluent

import scala.language.implicitConversions

import squid.utils._
import Embedding.Predef._

case class BoolOrLattice(private var b: Boolean)
  extends Embedding.SelfAssignedVariable[BoolOrLattice]
  with Lattice[BoolOrLattice]
{
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: BoolOrLattice): Option[Int] = {
    Some(b.compareTo(that.b))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: BoolOrLattice): BoolOrLattice = BoolOrLattice(b || that.b)
  def &&(that: BoolOrLattice): BoolOrLattice = BoolOrLattice(b && that.b)
  def ||(that: BoolOrLattice): BoolOrLattice = BoolOrLattice(b || that.b)
  def unary_! : BoolOrLattice = BoolOrLattice(!b)

  // Methods ///////////////////////////////////////////////////////////////////
  def <--(that: BoolOrLattice) = { b = that.b }
  def &&=(that: BoolOrLattice) = { b &&= that.b }
  def ||=(that: BoolOrLattice) = { b ||= that.b }
}
