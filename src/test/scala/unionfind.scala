package test.unionfind

import my.uf._

import org.scalatest.FunSuite

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
}
