package com.crossman.day06

import com.crossman.common.AdventApp

object Day06b extends AdventApp(6,"test") {
  override def program(): Unit = {
    /**
     * Every 7 days, the string of digits double in length as follows:
     * - sort the digits
     * - add two modulo nine to the digits
     */

    var code = List(3,4,3,1,2).toArray
    (0 until 256 by 7).foreach(day => {
      println(s"day: ${day}")

      // sort the digits
      // add two module nine
      val newCode = code.sorted.map(i => (i + 2) % 9)

      // build the new code string
      code = code ++ newCode
    })
    println(s"code: ${code}")
  }
}
