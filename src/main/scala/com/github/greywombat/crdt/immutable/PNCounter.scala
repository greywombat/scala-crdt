package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId

case class PNCounterOp(inc: Int)

object PNCounter extends ImmutableCRDTInit[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])] {
  def apply(count: Int)(implicit nodeId: NodeId) =
    new PNCounter(Map(nodeId -> math.max(0, count)), Map(nodeId -> math.min(0, count)))

  def empty = new PNCounter(Map.empty, Map.empty)

  override def combine(x: ImmutableCRDT[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])], y: ImmutableCRDT[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])]): ImmutableCRDT[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])] =
    new PNCounter(
      (x.state._1.toSeq ++ y.state._1.toSeq).groupBy(_._1).mapValues(_.map(_._2).sum),
      (x.state._2.toSeq ++ y.state._2.toSeq).groupBy(_._1).mapValues(_.map(_._2).sum))
}

/**
  * PNCounter is a counter that can be incremented or decremented.
  *
  * @param pCount
  * @param nCount
  */
class PNCounter(pCount: Map[NodeId, Int], nCount: Map[NodeId, Int]) extends ImmutableCRDT[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])] {
  override val state = (pCount, nCount)

  override def update(op: PNCounterOp)(implicit node: NodeId) =
    new PNCounter(
      pCount + ((node, pCount.getOrElse(node, 0) + math.max(0, op.inc))),
      nCount + ((node, nCount.getOrElse(node, 0) + math.min(0, op.inc))))

  override def get: Int = pCount.values.sum + nCount.values.sum

  override def merge(other: (Map[NodeId, Int], Map[NodeId, Int])) =
    PNCounter.combine(this, new PNCounter(other._1, other._2))

  /**
    * Increment the counter by one.
    *
    * @param nodeId This process identifier.
    * @return A new GCounter instance.
    */
  def inc(implicit nodeId: NodeId) = update(PNCounterOp(1))(nodeId)

  /**
    * Decrement the counter by one.
    *
    * @param nodeId This process identifier.
    * @return A new GCounter instance.
    */
  def dec(implicit nodeId: NodeId) = update(PNCounterOp(-1))(nodeId)

  /**
    * Increment or decrement the counter by inc.
    *
    * @param inc    Amount to increment (or decrement iff inc<0)
    * @param nodeId This process identifier.
    * @return A new GCounter instance.
    */
  def +(inc: Int)(implicit nodeId: NodeId) = update(PNCounterOp(inc))(nodeId)
}
