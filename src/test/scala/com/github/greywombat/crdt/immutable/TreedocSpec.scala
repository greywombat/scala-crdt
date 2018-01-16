package com.github.greywombat.crdt.immutable

import com.github.greywombat.crdt.NodeId
import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

object TreedocProps {
  implicitly[Arbitrary[Start]]
  implicitly[Arbitrary[Following]]
  implicitly[Arbitrary[Position]]
  implicitly[Arbitrary[TreedocInsert[Int]]]
  implicitly[Arbitrary[TreedocRemove]]
  implicitly[Arbitrary[TreedocOp[Int]]]
}

class TreedocProps extends PropSpec with TableDrivenPropertyChecks with Matchers {

  implicit val nodeId: NodeId = NodeId("testnode")

  val crdts = Table(
    ("crdt", "value"),
    (Treedoc.empty[Int], Seq.empty[Int]),
    (Treedoc.empty + 1, Seq(1)),
    (Treedoc.fromSeq(Seq(1, 2)), Seq(1, 2)),
    (Treedoc.empty.insert(0, 1), Seq(1)),
    ((Treedoc.empty + 2).remove(0), Seq.empty[Int]),
    (Treedoc.empty + 1 + 2 + 3 + 4 + 5, Seq(1, 2, 3, 4, 5))
  )

  property("combine should preserve elements") {
    forAll(crdts) { case (c1, value1) =>
      forAll(crdts) { case (c2, value2) =>
        Treedoc.combine(c1, c2).toSet should equal((value1 ++ value2).toSet)
      }
    }
  }

  property("combine should be commutative") {
    forAll(crdts) { case (c1, _) =>
      forAll(crdts) { case (c2, _) =>
        Treedoc.combine(c1, c2) should equal(Treedoc.combine(c2, c1))
      }
    }
  }

  property("merge and combine should be identical") {
    forAll(crdts) { case (c1, _) =>
      forAll(crdts) { case (c2, _) =>
        c1.merge(c2.state) should equal(Treedoc.combine(c1, c2))
      }
    }
  }

  val operations = Table(
    ("operation", "guard"),
    ((t: Treedoc[Int]) => t.insert(0, 2), (t: Treedoc[Int]) => true),
    ((t: Treedoc[Int]) => t.insert(1, 2), (t: Treedoc[Int]) => t.length >= 1),
    ((t: Treedoc[Int]) => t.remove(0), (t: Treedoc[Int]) => t.length >= 1),
    ((t: Treedoc[Int]) => t.remove(1), (t: Treedoc[Int]) => t.length >= 2)
  )

  property("merge and operation execution should be distributive") {
    forAll(crdts) { case (c1, _) =>
      forAll(crdts) { case (c2, _) =>
        forAll(operations) { case (operation, guard) =>
          if (guard(c1) && guard(c2)) {
            // have to use to set because operations insert and remove are not deterministic (involve random unique id)
            operation(c1).merge(c2.state).toSet should equal(operation(c1.merge(c2.state)).toSet)
          }
        }
      }
    }
  }
}

class TreedocSpec extends WordSpec {
  "An Treedoc" when {
    "empty" should {
      "be empty" in {
        assert(Treedoc.empty[Int].get == Seq())
      }
      "allow to insert an element at the beginning" in {
        assert(Treedoc.empty.insert(0, 10) == Seq(10))
      }
      "allow to append elements" in {
        assert(Treedoc.empty + 10 == Seq(10))
        assert(Treedoc.empty + 10 + 20 == Seq(10, 20))
      }
    }
    "containing values" should {
      "allow to insert values" in {
        assert(Treedoc.empty.insert(0, 10).insert(0, 20) == Seq(20, 10))
        assert(Treedoc.empty.insert(0, 10).insert(1, 20) == Seq(10, 20))
        assert(Treedoc.empty.insert(0, 10).insert(0, 20).insert(1, 15) == Seq(20, 15, 10))
      }
      "not allow to insert values out of bounds" in {
        assertThrows[IndexOutOfBoundsException](Treedoc.empty.insert(0, 10).insert(10, 20))
        assertThrows[IndexOutOfBoundsException](Treedoc.empty.insert(0, 10).insert(-1, 20))
      }
      "allow to append values" in {
        assert(Treedoc.empty.insert(0, 10) + 20 == Seq(10, 20))
      }
      "allow to retrieve elements by position" in {
        assert(Treedoc.empty.insert(0, 10).insert(0, 20)(1) == 10)
      }
      "allow to remove values by position" in {
        assert(Treedoc.empty.insert(0, 10).insert(0, 20).remove(0) == Seq(10))
        assert(Treedoc.empty.insert(0, 10).insert(0, 20).remove(1) == Seq(20))
      }
      "not allow to remove elements out of bounds" in {
        assertThrows[IndexOutOfBoundsException](Treedoc.empty.insert(0, 10).insert(0, 20).remove(10))
      }
    }
    "after elements have been removed" should {
      "allow to insert values" in {
        assert(Treedoc.empty.insert(0, 10).remove(0).insert(0, 20) == Seq(20))
      }
    }
    "merged" should {
      "not break concurrently inserted sequences" in {
        assert((Treedoc.empty + 1 + 2 + 3).merge((Treedoc.empty + 1 + 2 + 3).state) == Seq(1, 2, 3, 1, 2, 3))
        assert((Treedoc.empty + 1 + 2 + 3).remove(1).merge((Treedoc.empty + 1 + 3).state) == Seq(1, 3, 1, 3))
      }
      "preserve all elements" in {
        assert((Treedoc.empty + 1).insert(1, 2).merge((Treedoc.empty + 4 + 5 + 6).remove(2).state).sum == 12)
        assert(Treedoc.empty.insert(0, 1).insert(1, 2).remove(0).merge(Treedoc.empty.state).sum == 2)
      }
    }
  }
}
