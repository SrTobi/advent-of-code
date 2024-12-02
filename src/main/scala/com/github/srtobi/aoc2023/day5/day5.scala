package com.github.srtobi
package aoc2023.day5

import scala.annotation.tailrec


case class Mapping(source: Long, dest: Long, length: Long):
  def diff = dest - source
  def map(i: Long): Option[Long] =
    if i < source || i >= source + length
    then None
    else Some(i + diff)

  def sourceRange: SeedRange = SeedRange(source, length)

  def map(s: SeedRange): Option[(SeedRange, Seq[SeedRange])] =
    s.cutAway(sourceRange).map:
      case (r, rest) =>
        r.shift(diff) -> rest

object Mapping {
  def fromLine(line: String): Mapping =
    val Array(dest, source, length) = line.split(' ')
    Mapping(source.toLong, dest.toLong, length.toLong)
}

case class Mapper(from: String, to: String, mapping: Seq[Mapping]):
  def mapNum(i: Long): Long =
    mapping.iterator.map(_.map(i)).collectFirst:
      case Some(i) => i
    .getOrElse(i)

  def mapMulti(ranges: Seq[SeedRange], mappings: Seq[Mapping] = this.mapping): Seq[SeedRange] =
    mappings match
      case Seq() => ranges
      case m +: rest =>
        val (done, toDo) =
          ranges.map: r =>
            m.map(r) match
              case Some((s, toDo)) => Some(s) -> toDo
              case None => None -> Seq(r)
          .unzip
        done.flatten ++ mapMulti(toDo.flatten, rest)


object Mapper:
  def fromLines(lines: Seq[String]): Mapper =
    val Array(from, to) = lines.head.stripSuffix(" map:").split("-to-")
    val mappings = lines.tail.map(Mapping.fromLine)
    Mapper(from, to, mappings)


def cut(lines: Seq[String]): Seq[Seq[String]] =
  val (a, b) = lines.dropWhile(_.isBlank).span(s => !s.isBlank)
  if a.isEmpty
  then Seq.empty
  else a +: cut(b)

@main
def day5_task1(): Unit =
  val data = readData("assets/2023/day5/task1").linesIterator.toSeq

  val seeds = data.head.stripPrefix("seeds: ").split(' ').map(_.toLong).toSeq

  println(seeds)

  val blocks = cut(data.tail)
  val mappers = blocks.map(Mapper.fromLines).map(m => m.from -> m).toMap

  println(mappers.iterator.mkString("\n"))

  @tailrec
  def mapToLocation(i: Long, cur: String): Long =
    val mapper = mappers(cur)
    val mapped = mapper.mapNum(i)
    if mapper.to == "location"
    then mapped
    else mapToLocation(mapped, mapper.to)

  val results = seeds.map(mapToLocation(_, "seed"))

  println(results)
  println(results.min)
end day5_task1


case class SeedRange(start: Long, len: Long):
  assert(len >= 0)
  def end: Long = start + len
  def shift(i: Long): SeedRange = SeedRange(start + i, len)
  def cutAway(other: SeedRange): Option[(SeedRange, Seq[SeedRange])] =
    val maxStart = start max other.start
    val minEnd = end min other.end
    SeedRange.fromStartEnd(maxStart, minEnd).map:
      s =>
        s -> Seq(
          SeedRange.fromStartEnd(start, other.start),
          SeedRange.fromStartEnd(other.end, end)
        ).flatten

object SeedRange:
  def fromStartEnd(start: Long, end: Long): Option[SeedRange] =
    if start >= end
    then None
    else Some(SeedRange(start, end - start))

@main
def day5_task2(): Unit =
  val data = readData("assets/2023/day5/task1").linesIterator.toSeq

  val seeds = data.head.stripPrefix("seeds: ").split(' ')
    .sliding(2, 2)
    .map:
      case Array(a, b) => SeedRange(a.toLong, b.toLong)
    .toSeq

  println(seeds)

  val blocks = cut(data.tail)
  val mappers = blocks.map(Mapper.fromLines).map(m => m.from -> m).toMap

  println(mappers.iterator.mkString("\n"))

  @tailrec
  def mapToLocation(ranges: Seq[SeedRange], cur: String): Seq[SeedRange] =
    val mapper = mappers(cur)
    val mapped = mapper.mapMulti(ranges)
    println(s"after mapping from ${mapper.from} to ${mapper.to}:")
    println(mapped.mkString("\n"))
    if mapper.to == "location"
    then mapped
    else mapToLocation(mapped, mapper.to)

  val results = mapToLocation(seeds, "seed")

  println(results)
  println(results.map(_.start).min)

