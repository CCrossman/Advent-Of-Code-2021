package com.crossman.common

import scala.io.Source

abstract class AdventApp(day: Int, dat: String = "input") extends App {
  private val ordinal: String = if (day < 10) "0" + day else day.toString

  def program(): Unit

  protected val source = Source.fromFile(s"./src/main/resources/day${ordinal}/${dat}.dat")
  program()
  source.close()
}
