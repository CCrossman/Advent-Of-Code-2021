package com.crossman.day09

import com.crossman.common.{AdventApp, Board, IBoard}

object Day09a extends AdventApp(9) {

  override def program(): Unit = {
    val board = Board(source, IBoard.apply)
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
