package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

object MVRegisterProps {
  implicitly[Arbitrary[MVRegisterOp[Int]]]
}

class MVRegisterProps extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import Helpers._

  property("random interleaving of operation sequences yields equal result") {
    forAll { opSeq: List[(NodeId, List[Int])] =>
      val opSeq1 = randomOpsInterleaving(opSeq)
      val res1 = opSeq1.foldLeft(MVRegister.empty[Int]) { case (crdt, (node, op)) => crdt.set(op)(node) }
      val opSeq2 = randomOpsInterleaving(opSeq)
      val res2 = opSeq2.foldLeft(MVRegister.empty[Int]) { case (crdt, (node, op)) => crdt.set(op)(node) }
      res1 should equal(res2)
    }
  }

  property("merge operation is commutative and converges") {
    forAll { (state1: Set[MVRegisterOp[Int]], state2: Set[MVRegisterOp[Int]]) =>
      new MVRegister(state1).merge(state2) should equal(new MVRegister(state2).merge(state1))
    }
  }
}

class MVRegisterSpec extends WordSpec {
  implicit val nodeId = NodeId("testnode")

  "A MVRegister" when {
    "empty" should {
      "be empty" in {
        assert(MVRegister.empty[Int].get == Set.empty[Int])
      }
      "allow to overwrite value" in {
        assert((MVRegister.empty[Int].set(10)).get == Set(10))
      }
    }
    "holding concurrent values" should {
      val reg = MVRegister.empty[Int].set(1)(NodeId("node1")).update(MVRegisterOp(VectorClock(Map(NodeId("node2") -> BigInt(1))), 2))
      "yield multiple values" in {
        assert(reg.get.size == 2)
      }
      "allow to overwrite values" in {
        assert((reg.set(10)(NodeId("node1")).get == Set(10)))
      }
    }
  }
}
