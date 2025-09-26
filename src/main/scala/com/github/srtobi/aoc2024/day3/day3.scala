package com.github.srtobi.aoc2024.day3

import com.github.srtobi.readData

@main
def day3_task1(): Unit =
  val mulR = raw"mul\((\d+),(\d+)\)".r
  val regex = raw"mul\((\d+),(\d+)\)|do\(\)|don't\(\)".r
  val data = readData("assets/2024/day3/task1")

  println(
    regex.findAllIn(data).tapEach(println).foldLeft((0, true)) {
      case ((acc, true), mulR(a, b)) => (acc + a.toInt * b.toInt, true)
      case ((acc, _), m) if m == "do()" => (acc, true)
      case ((acc, _), m) if m == "don't()" => (acc, false)
      case ((acc, false), _) => (acc, false)
    }._1
  )