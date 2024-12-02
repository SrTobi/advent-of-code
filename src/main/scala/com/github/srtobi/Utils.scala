package com.github.srtobi

import com.github.srtobi.Coords.neighbourOffsets

import scala.io.Source


def readData(path: String): String =
  val source = Source.fromFile(path)
  try source.mkString finally source.close()


extension (coords: Coords)
  def x: Int = coords._1
  def y: Int = coords._2
  def +(other: Coords): Coords = (coords.x + other.x, coords.y + other.y)
  def -(other: Coords): Coords = (coords.x - other.x, coords.y - other.y)
  def unary_- : Coords = (-coords.x, -coords.y)
  def neighbours: Iterator[(Int, Int)] = neighbourOffsets.iterator.map(_ + coords)

type Coords = (Int, Int)

object Coords:
  val neighbourOffsets: Seq[Coords] =
    (-1 to 1).iterator.flatMap(y => (-1 to 1).map(_ -> y)).filterNot(_ == (0, 0)).toSeq

  def grid(width: Int, height: Int): Iterator[Coords] =
    for (x <- (0 until width).iterator; y <- 0 until height)
      yield (x, y)

extension [T](seq: Seq[T])
  def remove(idx: Int): Seq[T] = seq.take(idx) ++ seq.drop(idx + 1)