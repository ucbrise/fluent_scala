package fluent

trait Lattice[L] extends PartiallyOrdered[L] {
  def merge(that: L): L
}
