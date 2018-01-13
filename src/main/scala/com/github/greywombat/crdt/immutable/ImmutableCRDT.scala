package com.github.greywombat.crdt.immutable

import algebra.Eq
import algebra.lattice.{BoundedJoinSemilattice, JoinSemilattice}
import cats.kernel.Monoid
import com.github.greywombat.crdt.NodeId

/**
  *
  * @tparam T      The type of the content that the CRDT holds (e.g. Int for a counter).
  * @tparam OpT    The type of operations that the CRDT consumes and emits, and which determine its state.
  * @tparam StateT The type of the state of a CRDT. This is can be used to marshall or unmarshall a CRDT or merge two CRDTs.
  */
trait ImmutableCRDT[+T, OpT, StateT] {
  def state: StateT

  def update(input: OpT)(implicit node: NodeId): ImmutableCRDT[T, OpT, StateT]

  def get: T

  def merge(other: StateT): ImmutableCRDT[T, OpT, StateT]

  def canEqual(a: Any) = a.isInstanceOf[ImmutableCRDT[_, OpT, StateT]]

  override def equals(that: Any) = that match {
    case that: ImmutableCRDT[_, OpT, StateT] => that.canEqual(this) && this.get == that.get
    case _ => false
  }
}

trait ImmutableCRDTInit[T, OpT, StateT] {
  def empty: ImmutableCRDT[T, OpT, StateT]

  def combine(x: ImmutableCRDT[T, OpT, StateT], y: ImmutableCRDT[T, OpT, StateT]): ImmutableCRDT[T, OpT, StateT]

  implicit val mergeMonoid: Monoid[ImmutableCRDT[T, OpT, StateT]] with BoundedJoinSemilattice[ImmutableCRDT[T, OpT, StateT]] =
    ((that: ImmutableCRDTInit[T, OpT, StateT]) =>
      new Monoid[ImmutableCRDT[T, OpT, StateT]] with BoundedJoinSemilattice[ImmutableCRDT[T, OpT, StateT]] {
        override def empty: ImmutableCRDT[T, OpT, StateT] = that.empty

        override def combine(x: ImmutableCRDT[T, OpT, StateT], y: ImmutableCRDT[T, OpT, StateT]): ImmutableCRDT[T, OpT, StateT] =
          that.combine(x, y)

        override def zero: ImmutableCRDT[T, OpT, StateT] = empty

        override def join(lhs: ImmutableCRDT[T, OpT, StateT], rhs: ImmutableCRDT[T, OpT, StateT]): ImmutableCRDT[T, OpT, StateT] =
          combine(lhs, rhs)
      }) (this)

  implicit val eqGet: Eq[ImmutableCRDT[T, OpT, StateT]] =
    (x: ImmutableCRDT[T, OpT, StateT], y: ImmutableCRDT[T, OpT, StateT]) => x.get == y.get
}
