package com.crossman.day07

import com.crossman.common.AdventApp

object Day07a extends AdventApp(7) {
  override def program(): Unit = {
    val nums = source.getLines().next().split(',').map(_.toInt).toList
    //println(s"nums: ${nums}")

    val max = nums.max
    val sums = (0 to max).foldLeft(None: Option[(Int,Int)])((m,target) => {
      val sum = nums.map(n => Math.abs(n - target)).sum
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
