package com.crossman.day01

import scala.io.{BufferedSource, Source}

object Day01b extends App {
  val source: BufferedSource = Source.fromFile("./src/main/resources/day01/input.dat")

  val (_, count) = source.getLines().map(line => Integer.parseInt(line)).sliding(3).foldLeft((None: Option[Int], 0))((tuple,window) => {
    val (lastSum, count) = tuple
    val sum = window.sum

    if (lastSum.isEmpty) {
      (Some(sum), count)
    } else if (lastSum.get < sum) {
      (Some(sum),count + 1)
    } else {
      (Some(sum), count)
    }
  })

  println(s"count: ${count}")

  source.close()
}
