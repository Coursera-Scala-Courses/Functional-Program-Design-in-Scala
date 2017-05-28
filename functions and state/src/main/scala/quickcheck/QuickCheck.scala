package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math.min


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    isEmpty(h) ==> (findMin(insert(1, insert(2, h))) == 1)
  }

  property("gen3") = forAll { (h: H) =>
    isEmpty(h) ==> (deleteMin(insert(1, h)) == empty)
  }

  property("gen4") = forAll { (h: H) =>
    !isEmpty(h) ==> (auxFunction(h).sorted==auxFunction(h))
  }

  def auxFunction(h: H): List[A] = {
    if(isEmpty(h)) Nil
    else findMin(h)::auxFunction(deleteMin(h))
  }

  property("gen5") = forAll { (h1: H, h2: H) =>
    if(isEmpty(h1) && !isEmpty(h2))
      findMin(meld(h1,h2))==findMin(h2)
    else if(!isEmpty(h1) && isEmpty(h2))
      findMin(meld(h1,h2))==findMin(h1)
    else if(!isEmpty(h1) && !isEmpty(h2)){
      if(findMin(h1)<=findMin(h2))
        findMin(meld(h1,h2))==findMin(h1)
      else
        findMin(meld(h1,h2))==findMin(h2)
    }
    else true
  }

}
