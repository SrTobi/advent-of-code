package com.github.srtobi
package day4

import scala.collection.immutable.ArraySeq
import scala.collection.mutable


case class Card(id: Int, winningNumbers: Set[Int], numbers: Seq[Int]) {
  def winningHavs: Seq[Int] = numbers.filter(winningNumbers.contains)

  def matchingNumbers = winningHavs.length

  def worth: Int =
    winningHavs.foldLeft(0):
      case (0, _) => 1
      case (acc, _) => acc * 2
}

object Card:
  def fromString(s: String): Card =
    val Array(cardPart, numsPart) = s.split(':')
    val id = cardPart.stripPrefix("Card").trim.toInt
    val Array(winPart, havePart) = numsPart.split('|')
    val winNums = winPart.trim.split(raw"\s+").map(_.toInt).toSet
    val haveNums = havePart.trim.split(raw"\s+").map(_.toInt).toSeq
    Card(id, winNums, haveNums)

@main
def day4_task1(): Unit =
  val data = readData("assets/day4/task1")
  val cards = data.linesIterator.map(Card.fromString).toSeq
  println(cards.mkString("\n"))
  println(cards.map(_.worth).sum)


@main
def day4_task2(): Unit =
  val data = readData("assets/day4/task2")
  val cards = data.linesIterator.map(Card.fromString).to(ArraySeq)
  println(cards.map(c => s"$c -> ${c.worth}").mkString("\n"))
  val startMap = Map.from(cards.indices.map(_ -> 1))
  val resultMap = cards.indices.foldLeft(startMap):
    case (map, i) =>
      val n = map(i)
      val card = cards(i)
      ((i + 1) until ((i + 1) + card.matchingNumbers).min(cards.length)).foldLeft(map):
        case (map, i2) => map + (i2 -> (map(i2) + n))

  println(resultMap.toSeq.sortBy(_._1).mkString("\n"))
  println(resultMap.values.sum)
