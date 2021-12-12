package com.crossman.day12

import com.crossman.common.AdventApp

import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.mutable

object Day12a extends AdventApp(12) {
  sealed trait Cave {
    def label: String
  }
  case class BigCave(label: String) extends Cave
  case class SmallCave(label: String) extends Cave

  case class CaveSystem(caves: SortedSet[Cave], connections: scala.collection.immutable.Set[(Cave,Cave)])

  override def program(): Unit = {
    implicit val order: Ordering[Cave] = (x: Cave, y: Cave) => {
      if ("start".equals(x.label)) {
        -1
      } else if ("start".equals(y.label)) {
        1
      } else if ("end".equals(x.label)) {
        1
      } else if ("end".equals(y.label)) {
        -1
      } else {
        x.label.toLowerCase.compare(y.label.toLowerCase)
      }
    }

    val caves = mutable.HashSet.empty[Cave]
    val connections = mutable.HashSet.empty[(Cave,Cave)]

    source.getLines().foreach(line => {
      val parts = line.split('-')
      require(parts.length == 2)

      def toCave(s: String): Cave = {
        s match {
          case "end" => SmallCave("end")
          case "start" => SmallCave("start")
          case str if str(0).isLower => SmallCave(str)
          case str if str(0).isUpper => BigCave(str)
          case str => throw new IllegalArgumentException(s"unexpected line: '${str}'")
        }
      }

      val from = toCave(parts(0))
      val to = toCave(parts(1))

      caves.addOne(from)
      caves.addOne(to)
      connections.addOne(from -> to)
      connections.addOne(to -> from)
    })

    val caveSystem = CaveSystem(TreeSet.from(caves),connections.toSet)
    println(s"caveSystem: ${caveSystem}")

    case class Path(caves: Cave*)

    def backtrack(path: Path, cave: Cave): Set[Path] = {
      // reject 1
      if (path.caves.nonEmpty && !caveSystem.connections.contains(path.caves.last -> cave)) {
        return Set.empty
      }
      // reject 2
      if (path.caves.contains(cave) && cave.isInstanceOf[SmallCave]) {
        return Set.empty
      }
      val newPath = Path(path.caves.appended(cave): _*)
      // accept
      if (cave.label.equals("end")) {
        return Set(newPath)
      }
      val paths = scala.collection.mutable.Set.empty[Path]
      caveSystem.caves.foreach(cave => {
        paths.addAll(backtrack(newPath,cave))
      })
      paths.toSet
    }

    val paths = backtrack(Path(),SmallCave("start"))
    println(s"pathCount: ${paths.size}")
//    paths.foreach(path => {
//      println(path.caves.map(_.label).mkString(","))
//    })
  }
}
