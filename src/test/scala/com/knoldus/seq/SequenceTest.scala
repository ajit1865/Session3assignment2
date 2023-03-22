package com.knoldus.seq

import org.scalatest.flatspec.AnyFlatSpec

class SequenceTest extends AnyFlatSpec {

  val sequence = new Sequence

  it should "store a integer" in {
    assert(sequence.store(5) == 1)
  }

  it should "store strings" in {
    assert(sequence.store("yes, try") == 1)
  }

  it should "throw exception while storing Long " in {
    assertThrows[IllegalArgumentException](sequence.store(23L))
  }

  it should "throw exception while storing char " in {
    assertThrows[IllegalArgumentException](sequence.store('a'))
  }


}
