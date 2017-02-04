package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("retainingSmallestElement") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("removeSmallestElement") = forAll { (a: Int) =>
    if (a + 20 > a) {
      var x = a
      var h: H = empty
      while (x < a + 20) {
        h = insert(x, h)
        x = x + 1
      }
      findMin(deleteMin(h)) != findMin(h)
    } else true
  }

  property("meldingTwoHeaps") = forAll { (a: Int) =>
    if (a + 20 > a) {
      var x = a
      var h1: H = empty
      while (x < a + 10) {
        h1 = insert(x, h1)
        x = x + 1
      }
      var h2: H = empty
      while (x < a + 20) {
        h2 = insert(x, h2)
        x = x + 1
      }
      findMin(meld(h1, h2)) == math.min(findMin(h1), findMin(h2))
    } else true
  }
}
