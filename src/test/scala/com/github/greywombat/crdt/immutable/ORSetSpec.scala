package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

object ORSetProps {
  implicitly[Arbitrary[ORSetAdd[Int]]]
  implicitly[Arbitrary[ORSetRemove]]
  implicitly[Arbitrary[ORSetOp[Int]]]
  implicitly[Arbitrary[ORSetState[Int]]]
}

class ORSetProps extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import Helpers._

  property("random interleaving of operation sequences yields equal result") {
    forAll { opSeq: List[(NodeId, List[(Boolean, Int)])] =>
      val opSeq1 = randomOpsInterleaving(opSeq)
      val res1 = opSeq1.foldLeft(ORSet.empty[Int]) { case (crdt, (node, (add, value))) => if (add) crdt + value else crdt - value }
      val opSeq2 = randomOpsInterleaving(opSeq)
      val res2 = opSeq2.foldLeft(ORSet.empty[Int]) { case (crdt, (node, (add, value))) => if (add) crdt + value else crdt - value }
      res1 should equal(res2)
    }
  }

  property("merge operation is commutative and converges") {
    forAll { (state1: ORSetState[Int], state2: ORSetState[Int]) =>
      ORSet(state1).merge(state2) should equal(ORSet(state2).merge(state1))
    }
  }

  property("merge should be idempotent") {
    forAll { (state1: ORSetState[Int], state2: ORSetState[Int]) =>
      ORSet(state1).merge(state2).merge(state2) should equal(ORSet(state2).merge(state1))
    }
  }
}

class ORSetSpec extends WordSpec {
  implicit val nodeId = NodeId("testnode")

  "An ORSet" when {
    "empty" should {
      "be empty" in {
        assert(ORSet.empty[Int].get == Set.empty)
      }
      "allow to add an element" in {
        assert(ORSet.empty + 10 == Set(10))
      }
    }
    "containing values" should {
      "be iterable" in {
        assert((ORSet.empty + 1 + 2).map(_ * 2).sum == 6)
      }
      "allow to remove values" in {
        assert(ORSet.empty + 1 + 2 - 1 == Set(2))
      }
      "allow to add and remove values multiple times" in {
        assert(ORSet.empty + 1 + 2 - 1 - 2 + 1 == Set(1))
      }
    }
  }
}
