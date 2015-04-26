package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("insert two elements") = forAll { (a: Int, b: Int) =>
      val h = insert(a, empty)
      val res = insert(b, h)
      
      (a < b && findMin(res) == a) || (a > b && findMin(res) == b)
    }
  
  property("insert into empty heap then delete ") = forAll {a: Int =>
      val h = insert(a, empty)
      val res = deleteMin(h)
      
      isEmpty(res)
  }
  
  property("minimum of a meld") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    findMin(h3) == findMin(h1) || findMin(h3) == findMin(h2)
  }
  
  property("sorted list") = forAll { h: H =>
    def listMaker(h: H, res: List[Int]): List[Int] = isEmpty(h) match {
      case true => res
      case false => {
        val min = findMin(h)
        val newH = deleteMin(h)
        listMaker(newH, res ::: List(min))
      }
    }
    
    def isSorted(l: List[Int]): Boolean = l match {
      case Nil => true
      case x :: Nil => true
      case x :: xs => x <= xs.head && isSorted(xs)
    }
    
    isSorted(listMaker(h, List()))
    
  }  
  

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(v, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
