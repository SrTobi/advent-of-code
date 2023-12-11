package com.github.srtobi
package day3

import com.github.srtobi

import scala.collection.immutable.ArraySeq


class FieldNum(val num: Int) {
  override def toString: String = num.toString
}

enum GameField:
  case Num(fieldNum: FieldNum)
  case Symbol(c: Char)
  case Empty

type GameLine = ArraySeq[GameField]

object GameLine {
  def fromString(s: String): GameLine =
    var curField = Option.empty[FieldNum]
    s.indices.map:
      i =>
        s(i) match
          case '.' =>
            curField = None
            GameField.Empty
          case c if c.isDigit =>
            curField match
              case Some(field) => GameField.Num(field)
              case None =>
                val field = new FieldNum(s.substring(i).takeWhile(_.isDigit).toInt)
                curField = Some(field)
                GameField.Num(field)
          case c =>
            curField = None
            GameField.Symbol(c)
    .to(ArraySeq)
}

case class Game(lines: ArraySeq[GameLine]) {
  assert(lines.forall(_.length == lines.head.length))

  def at(x: Int, y: Int): Option[GameField] =
    lines.lift(y).flatMap(_.lift(x))

  def at(coord: (Int, Int)): Option[GameField] =
    at(coord._1, coord._2)

  def coords: Iterator[(Int, Int)] =
    for (x <- lines.head.indices.iterator; y <- lines.indices)
    yield (x, y)
}

object Game {
  def fromLines(lines: Iterator[String]): Game =
    Game(lines.map(GameLine.fromString).to(ArraySeq))
}

@main
def task1(): Unit =
  val game = Game.fromLines(readData("assets/day3/task1").linesIterator)
  println(game)
  println(game.coords.toSeq)
  val fields =
    for
      coords <- game.coords.toSeq
      if game.at(coords).exists(_.isInstanceOf[GameField.Symbol])
      n <- coords.neighbours
      case GameField.Num(field) <- game.at(n)
    yield field
  println(fields)
  println(fields.distinct.map(_.num).sum)


@main
def task2(): Unit =
  val game = Game.fromLines(readData("assets/day3/task2").linesIterator)

  val fields =
    for
      coords <- game.coords.toSeq
      if game.at(coords).contains(GameField.Symbol('*'))
    yield
      for
        n <- coords.neighbours.toSeq
        case GameField.Num(field) <- game.at(n)
      yield field
  println(fields)
  println(fields.map(_.distinct).filter(_.length == 2).map(l => l.head.num * l.last.num).sum)