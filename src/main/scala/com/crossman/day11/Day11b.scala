package com.crossman.day11

import com.crossman.common.{AdventApp, Board, MBoard}

import scala.collection.mutable

object Day11b extends AdventApp(11) {
  override def program(): Unit = {
    val board = Board(source, MBoard.apply).asInstanceOf[MBoard]
    //println(s"board: ${board}")

    var stepCount = 0
    while (!board.foldWithCoordinate(true)((isZero,xy) => isZero && board(xy._1,xy._2).contains(0))) {
      addOneStep(board)
      stepCount = stepCount + 1
    }

    println(s"stepCount: ${stepCount}")
  }

  private def addSteps(board: MBoard, numSteps: Int): Int = {
    var flashCount = 0
    (0 until numSteps).foreach($ => {
      flashCount = flashCount + addOneStep(board)
    })
    flashCount
  }

  private def addOneStep(board: MBoard): Int = {
    val updates = mutable.ArrayDeque.empty[(Int, Int)]
    val flashed = mutable.HashSet.empty[(Int, Int)]

    // increase each step by one
    board.forEachCoordinate((x, y) => {
      updates.append((x, y))
    })

    while (updates.nonEmpty) {
      val coordinate = updates.removeHead()
      val valueOption = board(coordinate._1, coordinate._2)
      //println(s"updating ${coordinate} from ${valueOption} to ${valueOption.map(_ + 1)}")
      valueOption.foreach(value => {
        if (value < 9) {
          board.update(coordinate._1, coordinate._2, value + 1)
        } else {
          // mark flashed
          board.update(coordinate._1, coordinate._2, Int.MinValue)
          flashed.addOne(coordinate._1, coordinate._2)

          // overflow
          board.forEachAdjacentDiagonal(coordinate._1, coordinate._2)((x, y) => {
            updates.addOne((x, y))
          })
        }
      })
    }

    var flashCount = 0
    val it = flashed.iterator
    while (it.hasNext) {
      val coordinate = it.next()
      board.update(coordinate._1, coordinate._2, 0)
      flashCount = flashCount + 1
    }
    flashed.clear()

    flashCount
  }
}
