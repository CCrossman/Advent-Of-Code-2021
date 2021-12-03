package com.crossman.day02

import scala.io.{BufferedSource, Source}

object Day02b extends App {
  val source: BufferedSource = Source.fromFile("./src/main/resources/day02/input.dat")

  val x = source.getLines().map(line => line.trim).foldLeft((0,0,0))((pos,line) => {
    pos match {
      case (forth, depth, aim) =>
        if (line.startsWith("forward ")) {
          val delta = line.substring("forward ".length).toInt
          (forth + delta, depth + (aim * delta), aim)
        } else if (line.startsWith("down ")) {
          val delta = line.substring("down ".length).toInt
          (forth, depth, aim + delta)
        } else if (line.startsWith("up ")) {
          val delta = line.substring("up ".length).toInt
          (forth, depth, aim - delta)
        } else {
          throw new IllegalArgumentException(s"unexpected line '${line}'")
        }
    }
  })

  println(s"x: ${x} => ${x._1 * x._2}")
}
