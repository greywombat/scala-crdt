package com.github.greywombat.crdt.immutable

import org.scalacheck.Arbitrary
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec, WordSpec}

object TreedocProps {
  implicitly[Arbitrary[Start]]
  implicitly[Arbitrary[Following]]
  implicitly[Arbitrary[Position]]
  implicitly[Arbitrary[TreedocInsert[Int]]]
  implicitly[Arbitrary[TreedocRemove]]
  implicitly[Arbitrary[TreedocOp[Int]]]
}

class TreedocProps extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

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
        assert((Treedoc.empty + 1).insert(1,2).merge((Treedoc.empty + 4 + 5 + 6).remove(2).state).sum == 12)
        assert(Treedoc.empty.insert(0,1).insert(1,2).remove(0).merge(Treedoc.empty.state).sum == 2)
      }
    }
  }
}
