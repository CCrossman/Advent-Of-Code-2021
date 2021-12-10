package com.crossman.day09

import com.crossman.common.AdventApp

object Day09a extends AdventApp(9) {
  case class Board(cells: List[List[Int]]) {
    def apply(x: Int, y: Int): Option[Int] = {
      if (cells.indices.contains(x)) {
        val ys = cells(x)
        if (ys.indices.contains(y)) {
          Some(ys(y))
        } else {
          None
        }
      } else {
        None
      }
    }

    def adjacentTo(x: Int, y: Int): List[Int] = {
      List(apply(x-1,y),apply(x+1,y),apply(x,y-1),apply(x,y+1)).flatMap(_.toList)
    }

    def foldWithCoordinate[Z](z: Z)(fn: (Z,(Int,Int)) => Z): Z = {
      var sum: Z = z
      cells.indices.foreach(x => {
        val ys = cells(x)
        ys.indices.foreach(y => {
          sum = fn(sum,(x,y))
        })
      })
      sum
    }
  }

  override def program(): Unit = {
    val board = Board(source.getLines().map(_.toCharArray.map(ch => ch - '0').toList).toList)
    println(s"board: ${board}")

    val lowpoints = board.foldWithCoordinate(Nil: List[Int])((z,xy) => {
      val x = xy._1
      val y = xy._2
      val n = board(x,y).get
      val adjacents = board.adjacentTo(x,y)
      if (adjacents.forall(i => i > n)) {
        z :+ n
      } else {
        z
      }
    })

    val risk = lowpoints.map(_ + 1).sum
    println(s"risk: ${risk}")
  }
}
