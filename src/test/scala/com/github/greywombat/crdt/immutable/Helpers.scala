package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId

object Helpers {
  def randomOpsInterleaving[OpT](opSeqs: List[(NodeId, List[OpT])]) =
    randomInterleaving(opSeqs.map { case (node, list) => list.map((node, _)) })

  def randomInterleaving[T](seqs: List[List[T]]): List[T] = {
    seqs.headOption match {
      case Some(seq) => seq.headOption match {
        case Some(e) => e :: randomInterleaving(seq.tail :: seqs.tail)
        case None => randomInterleaving(seqs.tail)
      }
      case None => Nil
    }
  }
}
