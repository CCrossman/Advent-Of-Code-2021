package com.crossman.day06

import com.crossman.common.AdventApp

object Day06a extends AdventApp(6,"test") {
  final case class Fish(seed: Int) {
    def forwardOneDay(): IterableOnce[Fish] = {
      if (seed == 0) {
        List(Fish(6),Fish(8))
      } else {
        List(Fish(seed - 1))
      }
    }
  }

  final case class Fishes(fish: Iterable[Fish]) {
    def count = fish.size

    def forward(howManyDays: Int): Fishes = {
      if (howManyDays == 0) {
        this
      } else {
        val fishes = fish.flatMap(f => f.forwardOneDay())
        Fishes(fishes).forward(howManyDays - 1)
      }
    }
  }

  override def program(): Unit = {
    var fish = Fishes(source.getLines().next().split(',').map(n => Fish(n.toInt)))
    fish = fish.forward(80)
    println(fish.count)
  }
}
