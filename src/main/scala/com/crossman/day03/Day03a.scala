package com.crossman.day03

import com.crossman.common.BinaryInt

import scala.io.{BufferedSource, Source}

object Day03a extends App {
  //val LEN = 5
  val LEN = 12

  //val source: BufferedSource = Source.fromFile("./src/main/resources/day03/test.dat")
  val source: BufferedSource = Source.fromFile("./src/main/resources/day03/input.dat")

  val gammaInBinary = source.getLines().map(line => line.trim).foldLeft(Array.fill(LEN)(0))((stats,line) => {
    val chars = line.toCharArray
    val updatedStats = chars.indices.foldLeft(stats)((arr,idx) => {
      chars(idx) match {
        case '0' =>
          arr.update(idx,arr(idx) - 1)
        case '1' =>
          arr.update(idx,arr(idx) + 1)
      }
      arr
    })
    updatedStats
  }).foldLeft("")((s,delta) => {
    if (delta < 0) {
      s + "0"
    } else if (delta > 0) {
      s + "1"
    } else {
      throw new IllegalArgumentException
    }
  })

  val gamma = BinaryInt(gammaInBinary).toDecimal
  val epsilon = BinaryInt(gammaInBinary).flip.toDecimal
  println(s"gamma: ${gamma}")
  println(s"epsilon: ${epsilon}")
  println(s"power rate: ${gamma * epsilon}")

  source.close()
}
