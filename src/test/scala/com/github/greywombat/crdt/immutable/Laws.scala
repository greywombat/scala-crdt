package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.util.Random

abstract class OpCRDTProps[T, OpT, StateT](crdt: ImmutableCRDTInit[T, OpT, StateT])(implicit gen: Gen[OpT])
  extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  /*property("random order of operation sequence yields equal results") {
    val opSeq = for (x <- Gen.listOf(gen)) yield x
    forAll(opSeq) { ops: List[OpT] =>
      val res1 = Random.shuffle(ops).foldLeft(crdt.empty)(_.update(_))
      val res2 = Random.shuffle(ops).foldLeft(crdt.empty)(_.update(_))
      res1.get should equal(res2.get)
    }
  }

  property("merge should be commutative") {
    val opSeq1 = for (x <- Gen.listOf(gen)) yield x
    val opSeq2 = for (x <- Gen.listOf(gen)) yield x
    forAll(opSeq1, opSeq2) { case (opSeq1, opSeq2) =>
      val res1 = Random.shuffle(opSeq1).foldLeft(crdt.empty)(_.update(_))
      val res2 = Random.shuffle(opSeq2).foldLeft(crdt.empty)(_.update(_))
      res1.merge(res2.state).get should equal(res2.merge(res1.state).get)
    }
  }

  property("Empty initialisation function should yield identity element") {
    val opSeq = for (x <- Gen.listOf(gen)) yield x
    forAll(opSeq) { ops: List[OpT] =>
      val res1 = Random.shuffle(ops).foldLeft(crdt.empty)(_.update(_))
      res1.merge(crdt.empty.state) should equal(res1)
    }
  }*/

  /*implicitly[Arbitrary[NodeId]]

  property("random interleaving of operation sequences yields equal result") {
    val opSeq: List[List[OpT]] = for (node <- Gen.resultOf(NodeId))
      for (opSeq <- Gen.listOf(gen)) yield (node, opSeq)

  }*/



}
