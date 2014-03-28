package scala.slick.util

final case class RefId[E <: AnyRef](e: E) {
  override def hashCode = System.identityHashCode(e)
  override def equals(o: Any) = o match {
    case RefId(e2) => e eq e2
    case _ => false
  }
  override def toString = "RefId("+e.toString+")@" + hashCode
  def apply() = e
}
