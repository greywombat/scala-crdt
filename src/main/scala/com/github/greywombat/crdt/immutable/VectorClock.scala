package com.github.greywombat.crdt.immutable

import algebra.lattice.BoundedJoinSemilattice
import cats.kernel.PartialOrder
import com.github.greywombat.crdt.NodeId

case class VectorClock(counters: Map[NodeId, BigInt]) extends PartiallyOrdered[VectorClock] {
  def inc(nodeId: NodeId) = new VectorClock(counters + ((nodeId, counters.getOrElse(nodeId, BigInt(0)) + 1)))

  override def tryCompareTo[B >: VectorClock <% PartiallyOrdered[B]](that: B): Option[Int] =
    that match {
      case that: VectorClock =>
        VectorClock.tryCompare(this, that)
      case _ => None
    }

  def merge(that: VectorClock) = VectorClock.join(this, that)
}

object VectorClock extends BoundedJoinSemilattice[VectorClock] with PartialOrder[VectorClock] {
  val empty = new VectorClock(Map.empty)

  override implicit val zero: VectorClock = empty

  override def join(lhs: VectorClock, rhs: VectorClock): VectorClock =
    new VectorClock((lhs.counters.toSeq ++ rhs.counters.toSeq).groupBy(_._1).mapValues(_.map(_._2).max))

  override def partialCompare(x: VectorClock, y: VectorClock): Double = {
    val xc = x.counters
    val yc = y.counters
    if (xc.forall { case ((node, count)) => count <= yc.getOrElse(node, BigInt(0)) } &&
      yc.exists { case ((node, count)) => count > xc.getOrElse(node, BigInt(0)) }) -1
    else if (yc.forall { case ((node, count)) => count <= xc.getOrElse(node, BigInt(0)) } &&
      xc.exists { case ((node, count)) => count > yc.getOrElse(node, BigInt(0)) }) 1
    else if (xc.filter(_._2 > BigInt(0)) == yc.filter(_._2 > BigInt(0))) 0
    else Double.NaN
  }

  def lteq(x: VectorClock, y: VectorClock): Boolean =
    x.counters.forall { case ((node, count)) => count <= y.counters.getOrElse(node, BigInt(0)) }

  def ineq(x: VectorClock, y: VectorClock): Boolean =
    tryCompare(x, y).isEmpty
}