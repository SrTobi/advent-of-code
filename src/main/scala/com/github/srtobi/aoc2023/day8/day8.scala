package com.github.srtobi
package aoc2023.day8

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

enum Instruction:
  case Left, Right

object Instruction:
  def fromChar(c: Char): Instruction =
    c match
      case 'L' => Instruction.Left
      case 'R' => Instruction.Right
      case c => throw new Exception(s"Unknown instruction ${c}")


case class Node(name: String, left: String, right: String):
  def goto(instruction: Instruction): String =
    instruction match
      case Instruction.Right => right
      case Instruction.Left => left

object Node:
  def fromLine(line: String): Node =
    val Array(name, rest) = line.split(" = ")
    val Array(left, right) = rest.replaceAll(raw"[()]", "").split(',')
    Node(name.trim, left.trim, right.trim)

@main
def day8_task1(): Unit =
  val Seq(instructionsTxt, _, rest*) = readData("assets/2023/day8/task1").trim.linesIterator.toSeq

  val instructions = instructionsTxt.map(Instruction.fromChar)
  println(instructions)
  val nodes = rest.map(Node.fromLine).map(n => n.name -> n).toMap

  println(nodes.values.mkString("\n"))


  @tailrec
  def run(cur: String, rest: Iterator[Instruction], num: Int = 0): Int =
    if cur.endsWith("Z") then num
    else rest.nextOption() match
      case Some(instr) => run(nodes(cur).goto(instr), rest, num + 1)
      case None => run(cur, instructions.iterator, num)

  val result = run("RKA", instructions.iterator)
  println(result)


@main
def day8_task2(): Unit =
  val Seq(instructionsTxt, _, rest: _*) = readData("assets/2023/day8/task1").trim.linesIterator.toSeq

  val instructions = instructionsTxt.map(Instruction.fromChar).to(ArraySeq)
  println(instructions)
  val nodes = rest.map(Node.fromLine).map(n => n.name -> n).toMap

  println(nodes.values.mkString("\n"))


  @tailrec
  def findCycles(cur: String, rawIdx: Int = 0, num: Long = 0, found: Seq[Long] = Seq.empty, visited: Map[(Int, String), Int] = Map.empty): (Seq[Long], Seq[Long]) =
    val idx = rawIdx % instructions.length
    if num > 0 && cur.endsWith("Z") then
      visited.get(idx -> cur) match
        case Some(i) => (found :+ num).splitAt(i)
        case None => findCycles(cur, idx, 0, found :+ num, visited + ((idx -> cur) -> (found.length)))
    else
      println(cur)
      findCycles(nodes(cur).goto(instructions(idx)), idx + 1, num + 1, found, visited)

  val cycles = nodes.keys.filter(_.endsWith("A")).map(findCycles(_)).toSeq

  class Goast(steps: Iterator[Long]):
    var dist = 0L

    def go(target: Long): Long =
      while dist < target
        do dist += steps.next()
      dist

  val goasts = cycles.map:
    (nonCycle, cycle) =>
      def endless: Iterator[Long] = cycle.iterator ++ endless
      Goast(nonCycle.iterator ++ endless)

  println(cycles.mkString("\n"))
  return

  def run(): Long =
    var maxDist = 1L
    var oldMax = 0L

    while oldMax != maxDist
    do
      oldMax = maxDist
      maxDist = goasts.iterator.map(_.go(maxDist)).max
      println(maxDist)
    maxDist

  println(run())
