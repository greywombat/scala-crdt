package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.WordSpec

object PNCounterProps {
  implicitly[Arbitrary[PNCounterOp]]
}

class PNCounterProps extends OpCRDTProps[Int, PNCounterOp, (Map[NodeId, Int], Map[NodeId, Int])](PNCounter)(Gen.resultOf(PNCounterOp)) {

  import Helpers._

  property("random interleaving of operation sequences yields equal result") {
    forAll { opSeq: List[(NodeId, List[Int])] =>
      val opSeq1 = randomOpsInterleaving(opSeq)
      val res1 = opSeq1.foldLeft(PNCounter.empty) { case (crdt, (node, inc)) => crdt.update(PNCounterOp(inc, node)) }
      val opSeq2 = randomOpsInterleaving(opSeq)
      val res2 = opSeq2.foldLeft(PNCounter.empty) { case (crdt, (node, inc)) => crdt.update(PNCounterOp(inc, node)) }
      res1 should equal(res2)
    }
  }

  property("merge operation is commutative and converges") {
    forAll { (state1: (Map[NodeId, Int], Map[NodeId, Int]), state2: (Map[NodeId, Int], Map[NodeId, Int])) =>
      PNCounter(state1._1, state1._2).merge(state2) should equal(new PNCounter(state2._1, state2._2).merge(state1))
    }
  }

  property("merge should be idempotent") {
    forAll { (state1: (Map[NodeId, Int], Map[NodeId, Int]), state2: (Map[NodeId, Int], Map[NodeId, Int])) =>
      PNCounter(state1._1, state1._2).merge(state2).merge(state2) should equal(PNCounter(state1._1, state1._2).merge(state2))
    }
  }
}

class PNCounterSpec extends WordSpec {
  implicit val nodeId = NodeId("testnode")

  "A PNCounter" when {
    "empty" should {
      "have value 0" in {
        assert(PNCounter.empty.get == 0)
      }
      "accept negative values" in {
        assert((PNCounter.empty + -1).get == -1)
      }
    }
  }
}
