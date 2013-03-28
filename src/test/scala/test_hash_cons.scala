package test.hashcons

import my.util._

import org.scalatest.FunSuite

class HashedString(val s:String) extends HashedType {
  def equal(other: HashedType) = {
    other match {
      case o:HashedString => s == o.s
      case _ => false
    }
  }
  def hash = s.hashCode()
}

class Suite extends FunSuite {
  test("Same value cons to the same tag") {
    val h = new HashCons[HashedString](4)
    val (a,aTag)  = h.hashCons(new HashedString("a"))
    val (b,bTag)  = h.hashCons(new HashedString("a"))
    assert(a.s == "a") 
    assert(b.s == "a") 
    assert(aTag == bTag)
    assert(a eq b)  // Object equality
  }

  test("Different values cons to different objects") {
    val h = new HashCons[HashedString](4)
    val (a,aTag)  = h.hashCons(new HashedString("a"))
    val (b,bTag)  = h.hashCons(new HashedString("b"))
    assert(a.s == "a")
    assert(b.s == "b")
    assert(aTag != bTag)
  }
}
