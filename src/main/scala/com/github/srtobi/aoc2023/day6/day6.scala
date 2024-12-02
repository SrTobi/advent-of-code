package com.github.srtobi
package aoc2023.day6

case class Race(time: Long, dist: Long):
  def minMax: (Double, Double) =
    val time = this.time.toDouble
    val max = time / 2.0 + math.sqrt(time * time / 4 - dist.toDouble)
    val min = time / 2.0 - math.sqrt(time * time / 4 - dist.toDouble)
    (min, max)

  def minMaxLong: (Long, Long) =
    val (min, max) = minMax
    ((min + 1).floor.toLong, (max - 1).ceil.toLong)


  def winPossibilities: Long =
    val (min, max) = minMaxLong
    max - min + 1

@main
def day6_task1(): Unit =
  val Seq(timeS, distS) = readData("assets/2023/day6/task1").trim.linesIterator.toSeq

  val times = timeS.stripPrefix("Time:").split(raw"\s+").filterNot(_.isBlank).map(_.toInt)
  val dists = distS.stripPrefix("Distance:").split(raw"\s+").filterNot(_.isBlank).map(_.toInt)

  val races = times.zip(dists).map((time, dist) => Race(time, dist))

  println(races.map(r => s"$r -> ${r.minMax}").mkString("\n"))
  println(races.map(_.winPossibilities).product)

@main
def day6_task2(): Unit =
  println(Race(49787980, 298118510661181L).minMaxLong)