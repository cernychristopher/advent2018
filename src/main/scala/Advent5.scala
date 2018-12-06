import scala.io.Source

object Advent5 {
  /*VERY SLOW:
  def react(s: String): String = {
    @scala.annotation.tailrec
    def combine(soFar: List[Char], next: List[Char]): List[Char] =
      next match {
        case a :: b :: tail if a.isLower && b.isUpper && b.toLower == a => combine(soFar, tail)
        case a :: b :: tail if a.isUpper && b.isLower && b.toUpper == a => combine(soFar, tail)
        case a :: b :: tail => combine(soFar ++ List(a), b :: tail)
        case x => soFar ++ next
      }

    combine(Nil, s.toList).mkString
  }*/

  private def subReact(s:String): String = if (s.isEmpty) "" else {
    s.grouped(2).filterNot(group => group.length == 2 && group(0).toLower == group(1).toLower && group(0).isLower != group(1).isLower).mkString
  }

  private def react(s:String): String = {
    val even = subReact(s)
    even.take(1) ++ subReact(even.drop(1))
  }

  private def reducedLength(s: String) = {
    val reducedInputs = Stream.iterate(s)(react)
    val reducedInputLengths = reducedInputs.map(_.length)
    reducedInputLengths.zip(reducedInputLengths.drop(1)).dropWhile(pair => pair._1 != pair._2).head._1
  }

  def main(args: Array[String]): Unit = {
    val testInput: String = Source.fromURL(getClass.getResource("/5.input")).getLines().next()
    val solution1 = reducedLength(testInput)
    println(solution1)

    val chars = testInput.map(_.toLower).toSet[Char].map(char => char -> testInput.filterNot(_.toLower == char)).toMap.mapValues(reducedLength)
    val solution2 = chars.minBy(_._2)
    println(solution2)
  }
}
