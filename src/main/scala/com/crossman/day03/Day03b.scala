package com.crossman.day03

import com.crossman.common.BinaryInt

import scala.io.{BufferedSource, Source}

object Day03b extends App {
  val LEN = 12
  val source: BufferedSource = Source.fromFile("./src/main/resources/day03/input.dat")

  //val LEN = 5
  //val source: BufferedSource = Source.fromFile("./src/main/resources/day03/test.dat")

  val values = source.getLines().map(line => BinaryInt(line.trim)).toList

  val oxygen = (0 until LEN).foldLeft(values)((remaining, idx) => {
    if (remaining.length > 1) {
      val counts = remaining.map(bi => bi(idx)).foldLeft((0, 0))((count, ch) => {
        val (x, y) = count
        ch match {
          case '0' => (x, y + 1)
          case '1' => (x + 1, y)
        }
      })

      if (counts._1 >= counts._2) {
        remaining.filter(bi => bi(idx) == '1')
      } else {
        remaining.filter(bi => bi(idx) == '0')
      }
    } else if (remaining.length == 1) {
      remaining
    } else {
      throw new IllegalArgumentException
    }
  })

  println(s"oxygen: ${oxygen}")

  val co2 = (0 until LEN).foldLeft(values)((remaining, idx) => {
    if (remaining.length > 1) {
      val counts = remaining.map(bi => bi(idx)).foldLeft((0, 0))((count, ch) => {
        val (x, y) = count
        ch match {
          case '0' => (x, y + 1)
          case '1' => (x + 1, y)
        }
      })

      if (counts._1 >= counts._2) {
        remaining.filter(bi => bi(idx) == '0')
      } else {
        remaining.filter(bi => bi(idx) == '1')
      }
    } else if (remaining.length == 1) {
      remaining
    } else {
      throw new IllegalArgumentException
    }
  })

  println(s"co2: ${co2}")
  println(s"life support: ${oxygen.head.toDecimal * co2.head.toDecimal}")

  source.close()
}
