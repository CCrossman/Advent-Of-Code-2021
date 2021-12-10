package com.crossman.day09

import com.crossman.common.AdventApp

import scala.collection.mutable

object Day09b extends AdventApp(9) {
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

    def get(x: Int, y: Int): Int = apply(x,y).get

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

  case class Basin(cells: List[(Int,Int)])

  override def program(): Unit = {
    val board = Board(source.getLines().map(_.toCharArray.map(ch => ch - '0').toList).toList)
    //println(s"board: ${board}")

    val lowpoints = board.foldWithCoordinate(Nil: List[(Int,Int)])((z,xy) => {
      val x = xy._1
      val y = xy._2
      val n = board.get(x,y)
      val adjacents = board.adjacentTo(x,y)
      if (adjacents.forall(i => i > n)) {
        z :+ (x,y)
      } else {
        z
      }
    })

    def addAdjacent(basins: List[Basin], x: Int, y: Int): List[Basin] = {
      val points = mutable.HashSet.empty[(Int,Int)]

      def addIfLow(x: Int, y: Int): Unit = {
        val hgt = board(x,y).getOrElse(9)
        if (hgt < 9 && !points.contains((x,y))) {
          points.addOne((x,y))

          addIfLow(x-1,y)
          addIfLow(x+1,y)
          addIfLow(x,y-1)
          addIfLow(x,y+1)
        }
      }

      addIfLow(x,y)

      basins :+ Basin(points.toList)
    }

    println(s"lowpoints: ${lowpoints}")
    val basins = lowpoints.foldLeft(Nil: List[Basin])((basins,xy) => {
      val (x,y) = xy
      addAdjacent(basins,x,y)
    })
    println(s"basins: ${basins}")

    println(basins.map(_.cells.length).sorted.reverse.take(3).product)
  }
}
