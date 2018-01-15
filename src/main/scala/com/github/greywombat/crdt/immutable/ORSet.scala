package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.{NodeId, UniqueId}

sealed trait ORSetOp[+T]

case class ORSetAdd[+T](id: UniqueId, value: T) extends ORSetOp[T]

case class ORSetRemove(id: UniqueId) extends ORSetOp[Nothing]

case class ORSetState[T](elems: Set[(UniqueId, T)], removed: Set[UniqueId])

object ORSet {
  //def apply[T](value: Set[T])(implicit nodeId: NodeId) = new LWWRegister[T](LWWRegisterOp(BigInt(1), nodeId, value))

  def empty[T]: ORSet[T] = new ORSet(ORSetState(Set.empty, Set.empty))

  def combine[T](x: ORSet[T], y: ORSet[T]): ORSet[T] =
    ORSet(ORSetState(x.state.elems ++ y.state.elems, x.state.removed ++ y.state.removed))
}

/**
  * An Observed removed set behaves like one would expect a set to behave.
  * The remove operation (-) removes exactly those elements present at the time when it was called.
  * Elements can be removed and added again arbitrarily many times.
  *
  * @param state
  * @tparam T The type of the content that the CRDT holds (e.g. Int for a counter).
  */
case class ORSet[T](state: ORSetState[T]) extends ImmutableCRDT[Set[T], ORSetOp[T], ORSetState[T]] with Set[T] {

  override def update(op: ORSetOp[T]): ORSet[T] = op match {
    case ORSetAdd(id, value) => ORSet(ORSetState(state.elems + ((id, value)), state.removed))
    case ORSetRemove(id) => ORSet(ORSetState(state.elems, state.removed + id))
  }

  override def get: Set[T] = state.elems.filterNot { case (id, _) => state.removed.contains(id) }.map(_._2)

  override def merge(other: ORSetState[T]) =
    ORSet(ORSetState(state.elems ++ other.elems, state.removed ++ other.removed))

  override def contains(elem: T): Boolean =
    state.elems.exists { case (id, e) => e == elem && !state.removed.contains(id) }

  override def +(elem: T): ORSet[T] = update(ORSetAdd(UniqueId(), elem))

  override def -(elem: T): ORSet[T] = ORSet(ORSetState(state.elems, state.removed ++ state.elems.filter { case (_, e) => e == elem }.map(_._1)))

  override def iterator: Iterator[T] = get.iterator

  override def canEqual(a: Any) = super.canEqual(a)

  override def equals(that: Any) = super.equals(that)
}
