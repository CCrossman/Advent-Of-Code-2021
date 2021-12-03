package com.crossman.common

case class BinaryInt(value: String) {
  def apply(idx: Int): Char = value(idx)

  def toDecimal: Int = {
    value.foldLeft(0)((i,ch) => {
      ch match {
        case '0' => i * 2
        case '1' => i * 2 + 1
      }
    })
  }

  def flip: BinaryInt = {
    val s = value.foldLeft("")((s,ch) => {
      ch match {
        case '0' => s + "1"
        case '1' => s + "0"
      }
    })
    BinaryInt(s)
  }
}
