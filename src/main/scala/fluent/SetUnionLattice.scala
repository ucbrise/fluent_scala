package fluent

import scala.language.implicitConversions

import squid.utils._
import Embedding.Predef._

case class SetUnionLattice[A: IRType](var xs: Set[A])
  extends Embedding.SelfAssignedVariable[SetUnionLattice[A]]
  with Lattice[SetUnionLattice[A]]
{
  def this() = this(Set())

  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: SetUnionLattice[A]): Option[Int] = {
    (xs.subsetOf(that.xs), xs == that.xs, that.xs.subsetOf(xs)) match {
      case (true, false, false) => Some(-1)
      case (false, true, false) => Some(0)
      case (false, false, true) => Some(1)
      case (false, false, false) => None
      case (_, _, _) => throw new IllegalStateException("")
    }
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.union(that.xs))
  }

  def filter(f: A => Boolean): SetUnionLattice[A] = {
    new SetUnionLattice(xs.filter(f))
  }

  def map[B: IRType](f: A => B): SetUnionLattice[B] = {
    new SetUnionLattice(xs.map(f))
  }

  def cross[B: IRType](that: SetUnionLattice[B]): SetUnionLattice[(A, B)] = {
    new SetUnionLattice(for (x <- xs; y <- that.xs) yield (x, y))
  }

  def union(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.union(that.xs))
  }

  def intersect(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.intersect(that.xs))
  }

  def diff(that: SetUnionLattice[A]): SetUnionLattice[A] = {
    new SetUnionLattice(xs.diff(that.xs))
  }

  def size(): IntMaxLattice = {
    IntMaxLattice(xs.size)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def <--(that: SetUnionLattice[A]) = {
    xs = that.xs
  }

  def +=(that: SetUnionLattice[A]) = {
    xs = xs.union(that.xs)
  }

  def -=(that: SetUnionLattice[A]) = {
    xs = xs.diff(that.xs)
  }
}
