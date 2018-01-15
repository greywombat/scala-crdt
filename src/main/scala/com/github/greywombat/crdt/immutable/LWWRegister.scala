package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId

case class LWWRegisterOp[T](version: BigInt, nodeId: NodeId, value: T)

object LWWRegister {
  def apply[T](value: T)(implicit nodeId: NodeId) = new LWWRegister[T](LWWRegisterOp(BigInt(1), nodeId, value))

  def empty[T]: LWWRegister[T] = new LWWRegister(LWWRegisterOp(BigInt(0), NodeId(""), null.asInstanceOf[T]))

  def combine[T](x: ImmutableCRDT[T, LWWRegisterOp[T], LWWRegisterOp[T]], y: ImmutableCRDT[T, LWWRegisterOp[T], LWWRegisterOp[T]]): ImmutableCRDT[T, LWWRegisterOp[T], LWWRegisterOp[T]] =
    if (x.state.version < y.state.version) LWWRegister(y.state)
    else if (x.state.version > y.state.version) LWWRegister(x.state)
    else if (x.state.nodeId < y.state.nodeId) LWWRegister(y.state)
    else if (x.state.nodeId > y.state.nodeId) LWWRegister(x.state)
    else LWWRegister(x.state)
}

/**
  * GCounter is a "grow only" counter. That means it can only be incremented.
  *
  * @param state
  */
case class LWWRegister[T](state: LWWRegisterOp[T]) extends ImmutableCRDT[T, LWWRegisterOp[T], LWWRegisterOp[T]] {

  override def update(op: LWWRegisterOp[T]): LWWRegister[T] =
    if (state.version < op.version) LWWRegister(op)
    else if (state.version > op.version) LWWRegister(state)
    else if (state.nodeId < op.nodeId) LWWRegister(op)
    else if (state.nodeId > op.nodeId) LWWRegister(state)
    else LWWRegister(state)

  override def get: T = state.value

  override def merge(other: LWWRegisterOp[T]) =
    LWWRegister.combine(this, new LWWRegister(other))

  def set(value: T)(implicit node: NodeId): LWWRegister[T] =
    update(LWWRegisterOp(state.version + 1, node, value))
}
