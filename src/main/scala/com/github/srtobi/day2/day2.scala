package com.github.srtobi.day2

import com.github.srtobi.day3.Game
import com.github.srtobi.readData

import scala.io.Source


type GameSet = Map[String, Int]

object GameSet:
  def fromString(s: String): GameSet =
    s.split(',')
      .map: s =>
        val Array(num, color) = s.trim.split(' ')
        color -> num.toInt
      .toMap

case class Game(id: Int, sets: Seq[GameSet])

object Game:
  def fromString(s: String): Game =
    val Array(pretext, data) = s.split(':')
    val Array("Game", id) = pretext.split(' ')
    val sets = data.split(';').map(GameSet.fromString)
    Game(id.toInt, sets)


extension (bag: GameSet)
  def isPossible(set: GameSet): Boolean =
    set.forall:
      case (color, count) => bag(color) >= count

  def isPossible(game: Game): Boolean =
    game.sets.forall(bag.isPossible)

  def power: Int = bag.values.product


@main
def day2_1(): Unit =
  val bag = Map(
    "red" -> 12,
    "green" -> 13,
    "blue" -> 14
  )

  val games = readData("assets/day2/task1")
    .linesIterator
    .map(Game.fromString)
    .toSeq

  val result =
    games
      .filter(bag.isPossible)
      .map(_.id)
      .sum

  println(result)


@main
def day2_2(): Unit =
  val games = readData("assets/day2/task2")
    .linesIterator
    .map(Game.fromString)
    .toSeq

  val result =
    games.map:
      game =>
        var newResult = Map.empty[String, Int].withDefault(_ => 0)
        game.sets
          .flatten
          .foreach:
            case (color, count) =>
              newResult += color -> (newResult(color) max count)
        println(s"$game -> $newResult")
        newResult

  println(result.map(_.power).sum)
