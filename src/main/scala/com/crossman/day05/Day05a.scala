package com.crossman.day05

import com.crossman.common.AdventApp

import scala.collection.mutable.ListBuffer

object Day05a extends AdventApp(5) {

  final case class Point(x: Int, y: Int)

  final case class Line(from: Point, to: Point) {
    def path: Iterator[Point] = {
      // WEIRD: consider diagonal lines only
      if (from.x != to.x && from.y != to.y) {
        return Iterator.empty
      }

      val points = ListBuffer.empty[Point]
      points.addOne(from)

      var lastPoint = from
      while (!lastPoint.equals(to)) {
        val deltaX = to.x - lastPoint.x
        val deltaY = to.y - lastPoint.y

        var nextPoint: Point = null

        if (deltaX == 0) {
          if (deltaY == 0) {
            println("should not happen")
          } else if (deltaY < 0) {
            nextPoint = Point(lastPoint.x, lastPoint.y - 1)
          } else {
            nextPoint = Point(lastPoint.x, lastPoint.y + 1)
          }
        } else if (deltaX < 0) {
          if (deltaY == 0) {
            nextPoint = Point(lastPoint.x - 1, lastPoint.y)
          } else if (deltaY < 0) {
            nextPoint = Point(lastPoint.x - 1, lastPoint.y - 1)
          } else {
            nextPoint = Point(lastPoint.x - 1, lastPoint.y + 1)
          }
        } else {
          if (deltaY == 0) {
            nextPoint = Point(lastPoint.x + 1, lastPoint.y)
          } else if (deltaY < 0) {
            nextPoint = Point(lastPoint.x + 1, lastPoint.y - 1)
          } else {
            nextPoint = Point(lastPoint.x + 1, lastPoint.y + 1)
          }
        }

        points.addOne(nextPoint)
        lastPoint = nextPoint
      }

      points.iterator
    }
  }

  final case class Board(side: Int) {
    private val cells = Array.fill(side * side)(0)

    def apply(x: Int, y: Int): Int = {
      require(x >= 0 && x < side)
      require(y >= 0 && y < side)
      cells(x * side + y)
    }

    def increment(x: Int, y: Int): Unit = {
      require(x >= 0 && x < side)
      require(y >= 0 && y < side)
      val idx = x * side + y
      cells.update(idx, cells(idx) + 1)
    }

    def countAboveOne: Int = {
      cells.count(_ > 1)
    }

    def printBoard(): Unit = {
      (0 until side).foreach(y => {
        (0 until side).foreach(x => {
          val count = apply(x,y)
          if (count == 0) {
            print(".")
          } else if (count < 0) {
            throw new IllegalArgumentException
          } else {
            print(count)
          }
        })
        println()
      })
    }
  }

  private def parseLine(line: String): Line = {
    val points = line.split("->").map(_.trim).map(s => {
      val parts = s.split(',').map(_.toInt)
      Point(parts(0),parts(1))
    })
    Line(points(0),points(1))
  }

  override def program(): Unit = {
    val lines = source.getLines().map(line => parseLine(line)).toList
    val size = lines.foldLeft(0)((size,line) => {
      Math.max(line.from.x + 1,Math.max(line.from.y + 1,Math.max(line.to.x + 1,Math.max(line.to.y + 1,size))))
    })
    val result = lines.foldLeft(Board(size))((board,line) => {
      line.path.foldLeft(board)((board,point) => {
        board.increment(point.x,point.y)
        board
      })
    })

    result.printBoard()
    println(result.countAboveOne)
  }
}
