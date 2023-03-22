package com.knoldus.set

import org.scalatest.flatspec.AnyFlatSpec

class SetClassTest extends AnyFlatSpec {
  val setClasses = new SetClass

  it should "add a new number" in {
    assert(setClasses.addElement(3) == 1)
    assert(setClasses.addElement(4) == 2)
  }

  it should "add a list of numbers " in {
    assert(setClasses.addElement(List(5, 6)) == 4)
  }

  it should "not add same elements" in {
    assert(setClasses.addElement(List(5, 6)) == 4)
  }

  it should "return all subsets from set" in {
    assert(setClasses.getSubSetList.size == 16)
    assert(setClasses.getSubSetList.contains(List(3, 5)))
  }

  it should "return true when given correct subset" in {
    assert(setClasses.verifyIfSubSet(List(3, 4)))
  }

  it should "return false when given correct subset" in {
    assert(!setClasses.verifyIfSubSet(List(3, 9)))
  }

  it should "return true when element is present" in {
    assert(setClasses.checkIfExists(5))
  }

  it should "return false when element is present" in {
    assert(!setClasses.checkIfExists(9))
  }

  it should "remove element from db" in {
    assert(setClasses.removeElement(4) == 3)
  }

  it should "return all odd elements" in {
    assert(setClasses.getOddElement.size == 2)
  }
}
