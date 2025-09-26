package com.github.srtobi
package aoc2024.day4

@main
def day4_task1(): Unit =
  val input = readData("assets/2024/day4/task2")
  val lines = input.linesIterator.toSeq

  assert(lines.forall(_.length == lines.head.length))

  val height = lines.length
  val width = lines.head.length

  def mkSearchRegexString(word: String): Seq[String] = {
    def withOffset(offset: Int): String =
      word.toSeq.intersperse(".".repeat(width + offset)).mkString


    Seq(word, withOffset(-1), withOffset(0), withOffset(1)).map(regex => s"(?=$regex)${word.head}")
  }

  val searchWord = "XMAS"
  val allSearchRegexStrings = mkSearchRegexString(searchWord) ++ mkSearchRegexString(searchWord.reverse)
  val allSearchRegex = allSearchRegexStrings.map(_.r)

  val text = lines.mkString("|")
  val occurrences = allSearchRegex.map(_.findAllIn(text).length).sum
  println(occurrences)


@main
def day4_task2(): Unit =
  val input = readData("assets/2024/day4/task2")
  val lines = input.linesIterator.toSeq

  assert(lines.forall(_.length == lines.head.length))

  val height = lines.length
  val width = lines.head.length

  def mkSearchRegexString(words: Seq[String]): Seq[String] = {
    words.intersperse(".".repeat(width - 2))
  }

  val base1 = Seq("M.S", ".A.", "M.S")
  val base2 = Seq("M.M", ".A.", "S.S")
  val patterns = Seq(base1, base1.map(_.reverse), base2, base2.reverse)
  val regexes = patterns.map(mkSearchRegexString).map(_.mkString)
  val text = lines.mkString("|")
  val occurrences = regexes.map { s =>
    val regex = s"(?=$s)${s.head}".r
    regex.findAllIn(text).length
  }.sum
  println(occurrences)

object Test {
  trait FieldExtractor1[T]:
    def convert(value: (String, T)): List[String]

  object FieldExtractor:
    given FieldExtractor1[Boolean]:
      def convert(value: (String, Boolean)): List[String] = List(value._1)
}