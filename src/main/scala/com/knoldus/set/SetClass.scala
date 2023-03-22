package com.knoldus.set

class SetClass {

  private var set = Set.empty[Int]

  // Add element into set,  return the size
  def addElement(elem: Int): Int = {
    set += elem
    set.size
  }

  // add the elements of the list to the set,  return the size
  def addElement(elements: List[Int]): Int = {
    set ++= elements
    set.size
  }

  // get all the subsets of the set(db), make sure to check the types.
  def getSubSetList: List[List[Int]] = {
    set.subsets.map(_.toList).toList
  }

  // verify it the given list is a subset of the db set.
  def verifyIfSubSet(list: List[Int]): Boolean = {
    list.forall(set.contains)
  }

  // verify if the element is present in the set.
  def checkIfExists(elem: Int): Boolean = {
    set.contains(elem)
  }

  // remove if the element is present in the set, return the size
  def removeElement(elem: Int): Int = {
    set -= elem
    set.size
  }

  // return all the odd elements from the set
  def getOddElement: Set[Int] = {
    set.filter(_ % 2 != 0)
  }

}
