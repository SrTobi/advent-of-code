package com.github.srtobi.aoc2024.day1

import com.github.srtobi.readData


@main
def day1_task1(): Unit =
  val lines = readData("assets/2024/day1/task1").linesIterator.toSeq
  val (left, right) = lines
    .map(_.split(' ').filter(_.nonEmpty).map(_.toInt))
    .map { case Array(left, right) => (left, right) }.unzip

  val sum = left.sorted
    .zip(right.sorted)
    .map(_ - _)
    .map(math.abs)
    .sum

  println(sum)


@main
def day1_task2(): Unit =
  val lines = readData("assets/2024/day1/task0").linesIterator.toSeq
  val (left, right) = lines
    .map(_.split(' ').filter(_.nonEmpty).map(_.toInt))
    .map { case Array(left, right) => (left, right) }.unzip

  val occurrences = right.groupBy(identity).view.mapValues(_.length).toMap.withDefaultValue(0)

  val sum = left.map(left => left * occurrences(left)).sum

  println(sum)

