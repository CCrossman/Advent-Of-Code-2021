package com.crossman.day13

import com.crossman.common.{AdventApp, MBoard}

object Day13a extends AdventApp(13) {
  override def program(): Unit = {
    val SIZE = 1500
    var board = MBoard(List.fill(SIZE,SIZE)(0))

    source.getLines().foreach(line => {
      if (line.startsWith("fold along y=")) {
        val x = line.substring("fold along y=".length).toInt
        val newBoard = MBoard(List.fill(SIZE,SIZE)(0))

        (0 until SIZE).foreach(iy => {
          (0 until x).foreach(ix => {
            newBoard.update(ix,iy,board.get(ix,iy))
          })
          // x = ix, no markers by rule
          ((x + 1) to (2 * x)).foreach(ix => {
            if (board.get(ix,iy) == 1) {
              newBoard.update((2 * x) - ix, iy, 1)
            }
          })
        })

        board = newBoard
        println(s"dots: ${board.foldWithCoordinate(0)((count,xy) => if (board.get(xy._1,xy._2) == 1) count + 1 else count)}")
      } else if (line.startsWith("fold along x=")) {
        val y = line.substring("fold along x=".length).toInt
        val newBoard = MBoard(List.fill(SIZE,SIZE)(0))

        (0 until SIZE).foreach(ix => {
          (0 until y).foreach(iy => {
            newBoard.update(ix,iy,board.get(ix,iy))
          })
          // y = iy, no markers by rule
          ((y + 1) to (2 * y)).foreach(iy => {
            if (board.get(ix,iy) == 1) {
              newBoard.update(ix, (2*y) - iy, 1)
            }
          })
        })

        board = newBoard
        println(s"dots: ${board.foldWithCoordinate(0)((count,xy) => if (board.get(xy._1,xy._2) == 1) count + 1 else count)}")
      } else if (line.isBlank) {
        // do nothing
        //board.println()
      } else {
        val parts = line.trim.split(',')
        require(parts.length == 2)

        val y = parts(0).toInt
        val x = parts(1).toInt

        board.update(x,y,1)
      }
    })

    board.println(10,50)
  }
}
