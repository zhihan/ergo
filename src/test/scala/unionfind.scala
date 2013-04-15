package test.unionfind

import my.uf._

import org.scalatest.FunSuite
import scala.math.Ordering

class Suite extends FunSuite {
  test("String list to equalities") {
    val l1 = List("1")
    val e1 = Formula.makeEqualities(l1)
    assert(e1.size == 0)

    val l2 = List("1", "2")
    val e2 = Formula.makeEqualities(l2)
    assert(e2.size == 1)

    val l3 = List("1", "2", "3")
    val e3 = Formula.makeEqualities(l3)
    assert(e3.size == 2)
  }

  test("Modulo 3") {
   // implicit int ordering defined in library 
    val c0 = UnionFind.empty[Int](scala.math.Ordering.Int)
    val c1 = c0.add(1).add(2).add(3).add(4).add(5).add(6)
    val c = c1.union(1,4).union(2,5).union(3,6)
    val c2 = c.distinct(1,2).distinct(1,3).distinct(2,3)
    assert(c2.areEqual(1,4))
    assert(c2.areEqual(2,5))
    assert(!c2.areEqual(4,5))
    assert(c2.areDistinct(4,5))
  }

  test("Modulo 3 explain") {
    val c0 = UnionFind.empty[Int](scala.math.Ordering.Int)
    val c1 = c0.add(1).add(2).add(3).add(4).add(5).add(6)
    val c = c1.union(1,4).union(2,5).union(3,6)
    val c2 = c.distinct(1,2).distinct(1,3).distinct(2,3)
    assert(c2.explain(1,4).size == 1)
    assert(c2.explainNeq(4,5).size == 3)
  }

}
