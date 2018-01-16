package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.{NodeId, UniqueId}

sealed trait Position

case class Start() extends Position

case class Following(pos: UniqueId) extends Position

case class TreedocElem[T](id: UniqueId, position: Position, value: T)

case class TreedocState[T](elems: Map[UniqueId, TreedocElem[T]],
                           following: Map[Position, Set[UniqueId]],
                           removed: Set[UniqueId])

sealed trait TreedocOp[+T]

case class TreedocInsert[+T](id: UniqueId, pos: Position, value: T) extends TreedocOp[T]

case class TreedocRemove(id: UniqueId) extends TreedocOp[Nothing]

object Treedoc {
  //def apply[T](value: Seq[T])(implicit nodeId: NodeId) = Treedoc.empty

  def empty[T]: Treedoc[T] = Treedoc(TreedocState(Map.empty, Map.empty, Set.empty))

  def combine[T](x: Treedoc[T], y: Treedoc[T]): Treedoc[T] =
    Treedoc(TreedocState[T](
      x.state.elems ++ y.state.elems,
      (x.state.following.toSeq ++ y.state.following.toSeq).groupBy(_._1).mapValues(_.map(_._2).reduce(_ union _)),
      x.state.removed ++ y.state.removed
    ))
}

case class Treedoc[T](state: TreedocState[T]) extends ImmutableCRDT[Seq[T], TreedocOp[T], TreedocState[T]] with Seq[T] {

  override def update(op: TreedocOp[T]): Treedoc[T] = op match {
    case TreedocInsert(id, pos, value) => Treedoc(TreedocState(
      state.elems + (id -> TreedocElem(id, pos, value)),
      state.following + (Following(id) -> state.following.getOrElse(pos, Set.empty)) + (pos -> Set(id)),
      state.removed
    ))
    case TreedocRemove(id) => Treedoc(TreedocState(
      state.elems,
      state.following,
      state.removed + id
    ))
  }

  private def traverse(from: Position): Stream[TreedocElem[T]] =
    state.following.getOrElse(from, Set()).toStream.sorted.map(state.elems).flatMap {
      case el@TreedocElem(id, _, _) => (if (state.removed.contains(id)) Stream() else Stream(el)) ++ traverse(Following(id))
    }

  override def get: Seq[T] = traverse(Start()).map(_.value)

  override def merge(other: TreedocState[T]) = Treedoc(TreedocState[T](
    state.elems ++ other.elems,
    (state.following.toSeq ++ other.following.toSeq).groupBy(_._1).mapValues(_.map(_._2).reduce(_ union _)),
    state.removed ++ other.removed
  ))

  override def length: Int = get.size

  override def iterator: Iterator[T] = traverse(Start()).map(_.value).iterator

  override def canEqual(a: Any) = super.canEqual(a)

  override def equals(that: Any) = super.equals(that)

  def +(a: T) =
    update(TreedocInsert(UniqueId(), traverse(Start()).lastOption.map(_.id).map(Following).getOrElse(Start()), a))

  def insert(idx: Int, value: T): Treedoc[T] = {
    val insertPos =
      if (idx == 0) Some(Start())
      else if (idx < 0) throw new IndexOutOfBoundsException
      else traverse(Start()).drop(idx - 1).headOption.map(_.id).map(Following)
    insertPos match {
      case Some(insertPos) => update(TreedocInsert(UniqueId(), insertPos, value))
      case None => throw new IndexOutOfBoundsException
    }
  }

  def apply(idx: Int) = get(idx)

  def remove(idx: Int): Treedoc[T] =
    update(TreedocRemove(traverse(Start())(idx).id))

}
