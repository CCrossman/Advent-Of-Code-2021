package com.crossman.day04

import com.crossman.common.AdventApp

import scala.collection.mutable

object Day04a extends AdventApp(4) {
  final case class Board(cells: List[Int], private val selected: scala.collection.mutable.Set[(Int,Int)] = new mutable.HashSet[(Int, Int)]()) {
    // row major
    def apply(x: Int, y: Int): Int = {
      require(x >= 0 && x < 5)
      require(y >= 0 && y < 5)
      cells(5 * x + y)
    }

    def find(value: Int): Option[(Int,Int)] = {
      (0 until 5).foreach(x => {
        (0 until 5).foreach(y => {
          if (value == apply(x,y)) {
            return Some((x,y))
          }
        })
      })
      None
    }

    def isSelected(x: Int, y: Int): Boolean = selected.contains((x,y))
    def select(x: Int, y: Int): Unit = selected.addOne((x,y))

    def select(value: Int): Boolean = {
      find(value) match {
        case Some((x,y)) =>
          select(x,y)
          true
        case None =>
          false
      }
    }

    def sumOfUnmarkedValues: Int = {
      var sum = 0
      (0 until 5).foreach(x => {
        (0 until 5).foreach(y => {
          if (!isSelected(x,y)) {
            sum = sum + apply(x,y)
          }
        })
      })
      sum
    }

    def isBingo: Boolean = {
      // check each row
      if (allSelected((0,0),(0,1),(0,2),(0,3),(0,4))) {
        return true
      }
      if (allSelected((1,0),(1,1),(1,2),(1,3),(1,4))) {
        return true
      }
      if (allSelected((2,0),(2,1),(2,2),(2,3),(2,4))) {
        return true
      }
      if (allSelected((3,0),(3,1),(3,2),(3,3),(3,4))) {
        return true
      }
      if (allSelected((4,0),(4,1),(4,2),(4,3),(4,4))) {
        return true
      }
      // check each column
      if (allSelected((0,0),(1,0),(2,0),(3,0),(4,0))) {
        return true
      }
      if (allSelected((0,1),(1,1),(2,1),(3,1),(4,1))) {
        return true
      }
      if (allSelected((0,2),(1,2),(2,2),(3,2),(4,2))) {
        return true
      }
      if (allSelected((0,3),(1,3),(2,3),(3,3),(4,3))) {
        return true
      }
      if (allSelected((0,4),(1,4),(2,4),(3,4),(4,4))) {
        return true
      }
      // check diagonals
//      if (allSelected((0,0),(1,1),(2,2),(3,3),(4,4))) {
//        return true
//      }
//      if (allSelected((0,4),(1,3),(2,2),(3,1),(4,0))) {
//        return true
//      }
      false
    }

    private def allSelected(positions: (Int,Int)*): Boolean = {
      positions.forall(position => {
        isSelected(position._1,position._2)
      })
    }
  }

  override def program(): Unit = {
    val lines = source.getLines()

    val selections = lines.next().split(',').map(_.toInt).toList
    require(lines.next().isEmpty)

    val boards = scala.collection.mutable.ListBuffer.empty[Board]
    while(lines.hasNext) {
      val cells = scala.collection.mutable.ListBuffer.empty[Int]
      (0 until 5).foreach(_ => cells.addAll(lines.next().split(' ').filter(_.nonEmpty).map(_.toInt)))
      boards.addOne(Board(cells.toList))

      if (lines.hasNext) {
        require(lines.next().isEmpty)
      }
    }

    selections.foreach(selection => {
      // mark selection on each board
      boards.foreach(board => {
        board.select(selection)

        // check for victory
        if (board.isBingo) {
          println(s"selection: ${selection}")
          println(s"board: ${board}")
          println(s"victory: ${board.sumOfUnmarkedValues * selection}")
          return
        }
      })
    })
//    println(s"selections: ${selections}")
//    println(s"boards: ${boards}")
  }
}
