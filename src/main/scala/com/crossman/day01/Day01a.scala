package com.crossman.day01

import scala.io.{BufferedSource, Source}

object Day01a extends App {
  val source: BufferedSource = Source.fromFile("./src/main/resources/day01/input.dat")

  val (_, count) = source.getLines().foldLeft((None: Option[Int],0))((tuple,line) => {
    val i = Integer.parseInt(line)
    val (lastValue: Option[Int], count: Int) = tuple

    if (lastValue.isEmpty) {
      (Some(i), count)
    } else if (lastValue.get < i) {
      (Some(i), count + 1)
    } else {
      (Some(i), count)
    }
  })

  println(s"count: ${count}")

  source.close()
}
