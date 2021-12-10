package com.crossman.day10

import com.crossman.common.AdventApp

import scala.collection.mutable

object Day10a extends AdventApp(10) {
  override def program(): Unit = {
    val openChars = "(<[{"
    val closeChars = ")>]}"

    val score = source.getLines().foldLeft(0)((score,line) => {
      val (_,illegalChar) = line.toCharArray.foldLeft((mutable.Stack.empty[Char], None: Option[Char]))((pair,char) => {
        //println(s"char: ${char}")
        val (stack,illegalChar) = pair

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
                println(s"Expected ${closeChars(openChars.indexOf(opener))}, but found ${char} instead.")
                (stack,Some(char))
              }
            } else {
              throw new UnsupportedOperationException
            }

          case s => (stack,s)
        }
      })

      score + (illegalChar match {
        case None => 0
        case Some(')') => 3
        case Some(']') => 57
        case Some('}') => 1197
        case Some('>') => 25137
      })
    })

    println(s"score: ${score}")
  }
}
