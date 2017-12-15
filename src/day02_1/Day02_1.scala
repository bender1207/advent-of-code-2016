package day02_1

import scala.io.Source

object Day02_1
{
  case class Position(x: Int, y: Int) {
    def valid() : Boolean = x >= 0 && x < 3 && y >= 0 && y < 3
    def +(other: Position) : Position = { Position(this.x + other.x, this.y + other.y) }
  }

  def toPosition(c: Char) : Position =
  {
    c match {
      case 'U' => Position(0, -1)
      case 'D' => Position(0, 1)
      case 'L' => Position(-1, 0)
      case 'R' => Position(1, 0)
    }
  }

  def toDigit(p: Position) : Int = p.x + p.y * 3 + 1
  def toPosition(d: Int) : Position = Position((d - 1) % 3, (d - 1) / 3)

  def findCode(rows: Seq[String]) : String =
  {
    buildString(rows.toList, Position(1, 1))
  }

  def getDigit(line: List[Char], p: Position) : Int =
  {
    line match {
      case Nil => toDigit(p)
      case c :: cs => {
        val dp = toPosition(c)
        val pNew = if ((p + dp).valid()) p + dp else p
        getDigit(cs, pNew)
      }
    }
  }

  def buildString(rows: List[String], startPos: Position) : String = {
    rows match {
      case Nil => ""
      case s :: ss => {
        val d = getDigit(s.toList, startPos)
        d.toString + buildString(ss, toPosition(d))
      }
    }
  }

  def main(args: Array[String]) =
  {
    //-- Tests -----------------
    {
      assert(findCode(Seq("ULL", "RRDDD", "LURDL", "UUUUD")) == "1985")
    }
    //--------------------------

    // Read input
    //
    val rows: Seq[String] = Source.fromResource("day02.txt").getLines.toSeq

    println(findCode(rows))
  }
}
