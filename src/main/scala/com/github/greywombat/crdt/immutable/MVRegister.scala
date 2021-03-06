package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId

case class MVRegisterOp[T](version: VectorClock, value: T)

object MVRegister {
  def apply[T](value: T)(implicit nodeId: NodeId) = new MVRegister(Set(new MVRegisterOp[T](VectorClock.empty.inc(nodeId), value)))

  def empty[T] = new MVRegister[T](Set.empty[MVRegisterOp[T]])

  def combine[T](x: ImmutableCRDT[Set[T], MVRegisterOp[T], Set[MVRegisterOp[T]]], y: ImmutableCRDT[Set[T], MVRegisterOp[T], Set[MVRegisterOp[T]]]): MVRegister[T] =
    new MVRegister(x.state.filterNot { case MVRegisterOp(xv, _) => y.state.exists(_.version > xv) }
      ++ y.state.filterNot { case MVRegisterOp(yv, _) => x.state.exists(_.version > yv) })
}

/**
  * A multi value register is a container for one value, that returns all concurrently (order of update cannot be determined) updates versions.
  *
  * @param state
  */
case class MVRegister[T](state: Set[MVRegisterOp[T]]) extends ImmutableCRDT[Set[T], MVRegisterOp[T], Set[MVRegisterOp[T]]] {

  override def update(op: MVRegisterOp[T]) =
    new MVRegister[T](state.filterNot(_.version < op.version) + op)

  override def get: Set[T] = state.map(_.value)

  override def merge(other: Set[MVRegisterOp[T]]): MVRegister[T] =
    MVRegister.combine(this, new MVRegister(other))

  def set(value: T)(implicit node: NodeId) =
    update(new MVRegisterOp(maxVersion.inc(node), value))

  val maxVersion = state.map(_.version).foldLeft(VectorClock.empty)(VectorClock.join)
}
