package com.crossman.common

import scala.io.BufferedSource

sealed trait Board {
  def adjacentTo(x: Int, y: Int): List[Int] = {
    List(apply(x-1,y),apply(x+1,y),apply(x,y-1),apply(x,y+1)).flatMap(_.toList)
  }

  def foldAdjacent[Z](z: Z, x: Int, y: Int)(fn: (Z,(Int,Int)) => Z): Z = {
    var sum: Z = z
    sum = fn(sum,(x-1,y))
    sum = fn(sum,(x+1,y))
    sum = fn(sum,(x,y-1))
    sum = fn(sum,(x,y+1))
    sum
  }

  def foldAdjacentDiagonal[Z](z: Z, x: Int, y: Int)(fn: (Z,(Int,Int)) => Z): Z = {
    var sum: Z = z
    sum = fn(sum,(x-1,y))
    sum = fn(sum,(x+1,y))
    sum = fn(sum,(x,y-1))
    sum = fn(sum,(x,y+1))
    sum = fn(sum,(x-1,y-1))
    sum = fn(sum,(x-1,y+1))
    sum = fn(sum,(x+1,y-1))
    sum = fn(sum,(x+1,y+1))
    sum
  }

  def foldWithCoordinate[Z](z: Z)(fn: (Z,(Int,Int)) => Z): Z = {
    var sum: Z = z
    indices.foreach(x => {
      val ys = row(x)
      ys.indices.foreach(y => {
        sum = fn(sum,(x,y))
      })
    })
    sum
  }

  def forEachCoordinate(fn: (Int,Int) => Unit): Unit = {
    indices.foreach(x => {
      val ys = row(x)
      ys.indices.foreach(y => {
        fn(x,y)
      })
    })
  }

  def forEachAdjacentDiagonal(x: Int, y: Int)(fn: (Int,Int) => Unit): Unit = {
    fn(x-1,y-1)
    fn(x-1,y)
    fn(x-1,y+1)
    fn(x,y-1)
    fn(x,y+1)
    fn(x+1,y-1)
    fn(x+1,y)
    fn(x+1,y+1)
  }

  def get(x: Int, y: Int): Int = apply(x,y).get
  def getOrElse(x: Int, y: Int, default: => Int): Int = apply(x,y).getOrElse(default)

  def println(): Unit = {
    indices.foreach(x => {
      val ys = row(x)
      ys.indices.foreach(y => {
        System.out.print(ys(y))
      })
      System.out.println()
    })
    System.out.println()
  }

  def println(mx: Int, my: Int): Unit = {
    (0 to mx).foreach(x => {
      val ys = row(x)
      (0 to my).foreach(y => {
        if (ys(y) == 1) {
          System.out.print('#')
        } else {
          System.out.print(' ')
        }
      })
      System.out.println()
    })
    System.out.println()
  }

  def apply(x: Int, y:Int): Option[Int]
  def indices: Iterable[Int]
  def row(x: Int): List[Int]
}

case class IBoard(cells: List[List[Int]]) extends Board {
  override def apply(x: Int, y: Int): Option[Int] = {
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
  override def indices: Iterable[Int] = cells.indices
  override def row(x: Int): List[Int] = cells(x)
}

case class MBoard(private var cells: List[List[Int]]) extends Board {
  override def apply(x: Int, y: Int): Option[Int] = {
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
  override def indices: Iterable[Int] = cells.indices
  override def row(x: Int): List[Int] = cells(x)

  def update(x: Int, y: Int, value: Int): Option[Int] = {
    if (cells.indices.contains(x)) {
      val ys = cells(x)
      if (ys.indices.contains(y)) {
        val oldValue = Some(ys(y))
        cells = cells.updated(x,ys.updated(y,value))
        oldValue
      } else {
        None
      }
    } else {
      None
    }
  }
}

object Board {
  def apply[B >: Board](source: BufferedSource, cons: List[List[Int]] => B): B = {
    cons(source.getLines().map(_.toCharArray.map(ch => ch - '0').toList).toList)
  }
}