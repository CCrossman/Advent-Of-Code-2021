package com.crossman.day07

import com.crossman.common.AdventApp

object Day07b extends AdventApp(7) {

  private def calcFuel(source: Int, target: Int): Int = {
    val n = Math.abs(source - target)
    n * (n + 1) / 2
  }

  override def program(): Unit = {
    val nums = source.getLines().next().split(',').map(_.toInt).toList
    //println(s"nums: ${nums}")

    val max = nums.max
    val sums = (0 to max).foldLeft(None: Option[(Int,Int)])((m,target) => {
      val sum = nums.map(n => Math.abs(calcFuel(n,target))).sum
      m match {
        case None => Some(target,sum)
        case Some((_,highest)) =>
          if (highest > sum) {
            Some(target,sum)
          } else {
            m
          }
      }
    })
    println(s"sums: ${sums}")
  }
}
