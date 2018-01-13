package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.WordSpec

object GCounterProps {
  implicitly[Arbitrary[GCounterOp]]
}

class GCounterProps extends OpCRDTProps[Int, GCounterOp, Map[NodeId, Int]](GCounter)(Gen.resultOf(GCounterOp)) {

  import Helpers._

  property("random interleaving of operation sequences yields equal result") {
    forAll { opSeq: List[(NodeId, List[Int])] =>
      val opSeq1 = randomOpsInterleaving(opSeq)
      val res1 = opSeq1.foldLeft(GCounter.empty) { case (crdt, (node, inc)) => crdt.update(GCounterOp(inc, node)) }
      val opSeq2 = randomOpsInterleaving(opSeq)
      val res2 = opSeq2.foldLeft(GCounter.empty) { case (crdt, (node, inc)) => crdt.update(GCounterOp(inc, node)) }
      res1 should equal(res2)
    }
  }
}

class GCounterSpec extends WordSpec {
  implicit val nodeId = NodeId("testnode")

  "A GCounter" when {
    "empty" should {
      "have value 0" in {
        assert(GCounter.empty.get == 0)
      }
      "not accept negative values" in {
        assert((GCounter.empty + (-1)).get == 0)
      }
    }
  }
}
