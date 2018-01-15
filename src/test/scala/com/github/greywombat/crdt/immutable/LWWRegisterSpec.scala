package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

object LWWRegisterProps {
  implicitly[Arbitrary[LWWRegisterOp[Int]]]
}

class LWWRegisterProps extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import Helpers._

  property("random interleaving of operation sequences yields equal result") {
    forAll { opSeq: List[(NodeId, List[Int])] =>
      val opSeq1 = randomOpsInterleaving(opSeq)
      val res1 = opSeq1.foldLeft(LWWRegister.empty[Int]) { case (crdt, (node, op)) => crdt.set(op)(node) }
      val opSeq2 = randomOpsInterleaving(opSeq)
      val res2 = opSeq2.foldLeft(LWWRegister.empty[Int]) { case (crdt, (node, op)) => crdt.set(op)(node) }
      res1 should equal(res2)
    }
  }

  property("merge operation is commutative and converges") {
    forAll { (state1: LWWRegisterOp[Int], state2: LWWRegisterOp[Int]) =>
      new LWWRegister(state1).merge(state2) should equal(new LWWRegister(state2).merge(state1))
    }
  }

  property("merge should be idempotent") {
    forAll { (state1: LWWRegisterOp[Int], state2: LWWRegisterOp[Int]) =>
      new LWWRegister(state1).merge(state2).merge(state2) should equal(new LWWRegister(state1).merge(state2))
    }
  }
}

class LWWRegisterSpec extends WordSpec {
  implicit val nodeId = NodeId("testnode")

  "A LWWRegister" when {
    "empty" should {
      "be empty" in {
        assert(LWWRegister.empty[String].get == null)
      }
      "allow to overwrite value" in {
        assert((LWWRegister.empty[Int].set(10)).get == 10)
      }
    }
  }
}
