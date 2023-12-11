package com.github.srtobi
package day7

import com.github.srtobi.day7.HandRank._

enum HandRank:
  case FiveOfAKind
  case FourOfAKind
  case FullHouse
  case ThreeOfAKind
  case TwoPair
  case OnePair
  case HighCard

object HandRank:
  given Ordering[HandRank] = Ordering.by(_.ordinal)

case class Hand(cards: Seq[Int]):
  lazy val rank: HandRank =
    val grouped = cards.groupBy(identity)
    lazy val size = grouped.size
    lazy val pairs = grouped.count((_, l) => l.length == 2)

    if size == 1 then FiveOfAKind
    else if grouped.exists((_, l) => l.length == 4) then FourOfAKind
    else if grouped.exists((_, l) => l.length == 3) then
      if size == 2 then FullHouse else ThreeOfAKind
    else if pairs > 0 then
      if pairs == 2 then TwoPair else OnePair
    else
      HighCard

  lazy val rankWithJ: HandRank =
    val jokers = cards.count(_ == Hand.cards('J'))
    val grouped = cards.filterNot(_ == Hand.cards('J')).groupBy(identity)
    lazy val size = grouped.size
    lazy val pairs = grouped.count((_, l) => l.length == 2)

    if size <= 1 then FiveOfAKind
    else if grouped.exists((_, l) => l.length + jokers == 4) then FourOfAKind
    else if grouped.exists((_, l) => l.length + jokers == 3) then
      if size == 2 then FullHouse else ThreeOfAKind
    else if pairs > 0 || jokers > 0 then
      if pairs == 2 || (pairs == 1 && jokers > 0) then TwoPair else OnePair
    else
      assert(jokers == 0, s"jokers == 0, failed for $this (size = $size, pairs = $pairs, grouped = $grouped)")
      assert(size + jokers == 5, s"Failed for ${this}")
      HighCard

object Hand:
  val cards: Map[Char, Int] = Map(
    '2' -> 2,
    '3' -> 3,
    '4' -> 4,
    '5' -> 5,
    '6' -> 6,
    '7' -> 7,
    '8' -> 8,
    '9' -> 9,
    'T' -> 10,
    'J' -> 1,
    'Q' -> 12,
    'K' -> 13,
    'A' -> 14
  )

  def fromString(s: String): Hand =
    assert(s.length == 5)
    Hand(s.map(cards))

  private val byRankWithoutJ: Ordering[Hand] = Ordering.by(_.rank)
  private val byRankWithJ: Ordering[Hand] = Ordering.by(_.rankWithJ)
  import Ordering.Implicits._
  val withoutJ: Ordering[Hand] = byRankWithoutJ.reverse.orElseBy(_.cards)
  val withJ: Ordering[Hand] = byRankWithJ.reverse.orElseBy(_.cards)

@main
def day7_task1(): Unit =
  assert(Hand.cards('J') == 11)
  val handToBet = readData("assets/day7/task1").trim
    .linesIterator
    .toSeq
    .map:
      line =>
        val Array(cards, bet) = line.split(raw"\s+")
        Hand.fromString(cards) -> bet.toInt
    .sortBy(_._1)(Hand.withoutJ)
  println(handToBet.map((hand, bet) => s"$hand -> ${hand.rank} -> ${bet}").mkString("\n"))

  println(handToBet.map(_._2).zipWithIndex.map((bet, idx) => bet * (idx + 1)).sum)


@main
def day7_task2(): Unit =
  assert(Hand.cards('J') == 1)
  val handToBet = readData("assets/day7/task1").trim
    .linesIterator
    .toSeq
    .map:
      line =>
        val Array(cards, bet) = line.split(raw"\s+")
        Hand.fromString(cards) -> bet.toInt
    .sortBy(_._1)(Hand.withJ)
  println(handToBet.map((hand, bet) => s"$hand -> ${hand.rankWithJ} -> ${bet}").mkString("\n"))

  println(handToBet.map(_._2).zipWithIndex.map((bet, idx) => bet * (idx + 1)).sum)

