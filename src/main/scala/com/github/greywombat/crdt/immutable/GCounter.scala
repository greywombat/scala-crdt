package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId

case class GCounterOp(inc: Int, node: NodeId)

object GCounter extends ImmutableCRDTInit[Int, GCounterOp, Map[NodeId, Int]] {
  def apply(count: Int)(implicit nodeId: NodeId) = new GCounter(Map(nodeId -> math.max(0, count)))

  override def empty = new GCounter(Map.empty)

  override def combine(x: ImmutableCRDT[Int, GCounterOp, Map[NodeId, Int]], y: ImmutableCRDT[Int, GCounterOp, Map[NodeId, Int]]): ImmutableCRDT[Int, GCounterOp, Map[NodeId, Int]] =
    new GCounter((x.state.toSeq ++ y.state.toSeq).groupBy(_._1).mapValues(_.map(_._2).max))
}

/**
  * GCounter is a "grow only" counter. That means it can only be incremented.
  *
  * @param state
  */
class GCounter(val state: Map[NodeId, Int]) extends ImmutableCRDT[Int, GCounterOp, Map[NodeId, Int]] {
  override def update(op: GCounterOp) =
    new GCounter(state + ((op.node, state.getOrElse(op.node, 0) + op.inc)))


  override def get: Int = state.values.sum

  override def merge(other: Map[NodeId, Int]) =
    GCounter.combine(this, new GCounter(other))

  /**
    * Increment the counter by one.
    *
    * @param nodeId This process identifier.
    * @return A new GCounter instance.
    */
  def inc(implicit nodeId: NodeId) = update(GCounterOp(1, nodeId))

  /**
    * Increment the counter by inc.
    *
    * @param inc    Amount to increment (must be >= 0).
    * @param nodeId This process identifier.
    * @return A new GCounter instance.
    */
  def +(inc: Int)(implicit nodeId: NodeId) = update(GCounterOp(math.max(0, inc), nodeId))
}
