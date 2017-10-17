package fluent

import scala.language.implicitConversions

import squid.utils._

object Embedding extends squid.ir.SimpleAST
import Embedding.Predef._
import Embedding.Quasicodes._

case class BoolOrLattice(var name: String, var b: Boolean) extends Lattice[BoolOrLattice] {
  // Ordering //////////////////////////////////////////////////////////////////
  def tryCompareTo(that: BoolOrLattice): Option[Int] = {
    Some(b.compareTo(that.b))
  }

  // Functions /////////////////////////////////////////////////////////////////
  def merge(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(name, b || that.b)
  }

  def &&(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(name, b && that.b)
  }

  def ||(that: BoolOrLattice): BoolOrLattice = {
    BoolOrLattice(name, b || that.b)
  }

  // Methods ///////////////////////////////////////////////////////////////////
  def &&=(that: BoolOrLattice) = {
    b &&= that.b
  }

  def ||=(that: BoolOrLattice) = {
    b ||= that.b
  }
}

object BoolOrLattice {
  def myvar[T](name: String): T = ???

  implicit def toCode(b: BoolOrLattice): IR[BoolOrLattice, Any] = {
    ir{myvar[BoolOrLattice](${Const(b.name)})}
  }
}
