package com.knoldus.map

import org.scalatest.flatspec.AnyFlatSpec

class MapClassTest extends AnyFlatSpec {

  val mapClass = new MapClass

  val map = Map("value1"-> "one")

  it should "return null storage" in {
    assert(mapClass.getAllValues.isEmpty)
  }

  it should "It should Store new mapped values" in{
    mapClass.store(map)
    val map2 = Map("value2"-> "two")
    val response = mapClass.store(map2)
    assert(response == 2)
  }

  it should "null values if key is not present" in {
    assertThrows[NoSuchElementException]{
      assert(mapClass.getValue("null").isEmpty)
    }
  }

  it should "return value if key is present" in {
    assert(mapClass.getValue("value1") == "one")
  }

  it should "not update value if key is not present" in {
    assert(mapClass.updateMap("value", "no") == 2)
  }

  it should "update value if key is present" in {
    assert(mapClass.updateMap("value1", "new") == 2)
    assert(mapClass.getValue("value1") == "new")
  }

  it should "return values with odd keys" in {
    val map3 = Map("1" -> "try me")
    mapClass.store(map3)
    assert(mapClass.getOddValue.size == 1)
  }

  it should "get null values if key is not present" in {
    assert(mapClass.getValuesSafely("3").isEmpty)
  }

}
