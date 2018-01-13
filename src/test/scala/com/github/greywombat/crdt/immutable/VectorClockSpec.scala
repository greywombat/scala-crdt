package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.ScalacheckShapeless._

class VectorClockSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  implicitly[Arbitrary[NodeId]]
  Arbitrary(Gen.resultOf((m: Map[NodeId, BigInt]) => VectorClock(m.mapValues(_.abs))))

  property("neutral element preserves identity") {
    forAll { clock: VectorClock =>
      assert(clock <= clock.merge(VectorClock.empty))
    }
  }

  property("a clock should be greater after increment") {
    forAll { clock: VectorClock =>
      forAll { nodeId: NodeId =>
        assert(clock < clock.inc(nodeId))
      }
    }
  }

  property("the merge operation should be monotonic") {
    forAll { (clock1: VectorClock, clock2: VectorClock) =>
      assert(clock1.merge(clock2) >= clock1)
    }
  }

  property("the merge operation should be commutative") {
    forAll { clock1: VectorClock =>
      forAll { clock2: VectorClock =>
        assert(clock1.merge(clock2) == clock2.merge(clock1))
      }
    }
  }
}
