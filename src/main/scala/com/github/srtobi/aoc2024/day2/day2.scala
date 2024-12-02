package com.github.srtobi
package aoc2024.day2

@main
def day2_task1(): Unit = {
  val lines = readData("assets/2024/day2/task1")
    .linesIterator
    .map(_.split(raw"\s").toSeq.map(_.toInt))
    .toSeq

  val safeReports =
    lines.count: levels =>
      levels.indices.map(levels.remove).exists: levels =>
        levels.sliding(2).forall { case Seq(a, b) => (a - b) > 0 && (a - b) <= 3 } ||
          levels.sliding(2).forall { case Seq(a, b) => (a - b) < 0 && (a - b) >= -3 }

  println(safeReports)
}
