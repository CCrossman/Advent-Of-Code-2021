package com.crossman.day10

import com.crossman.common.AdventApp

import scala.collection.mutable

object Day10b extends AdventApp(10) {
  override def program(): Unit = {
    val openChars = "(<[{"
    val closeChars = ")>]}"

    val scores = source.getLines().foldLeft(Nil: List[BigInt])((scores,line) => {
      val (stack,illegalChar) = line.toCharArray.foldLeft((mutable.Stack.empty[Char], None: Option[Char]))((pair,char) => {
        val (stack,illegalChar) = pair
        //println(s"stack: ${stack}")
        //println(s"char: ${char}")

        illegalChar match {
          case None =>
            if (openChars.contains(char)) {
              stack.push(char)
              (stack,None)
            } else if (closeChars.contains(char)) {
              val opener = stack.pop()

              if (closeChars.indexOf(char) == openChars.indexOf(opener)) {
                (stack,None)
              } else {
                //println(s"Expected ${closeChars(openChars.indexOf(opener))}, but found ${char} instead.")
                stack.push(opener)
                (stack,Some(char))
              }
            } else {
              throw new UnsupportedOperationException
            }

          case s => (stack,s)
        }
      })

      println(s"stack: ${stack}")

      scores appendedAll (illegalChar match {
        case None =>
          var score: BigInt = 0

          while (stack.nonEmpty) {
            val opener = stack.pop()
            score = (5 * score) + (opener match {
              case '(' => 1
              case '[' => 2
              case '{' => 3
              case '<' => 4
            })
          }

          Some(score)
        case _ =>
          None
      })
    })

    val scoreList = scores.sorted.toList
    println(s"scores: ${scoreList}")
    println(s"middle score: ${scoreList(scoreList.length / 2)}")
  }
}
