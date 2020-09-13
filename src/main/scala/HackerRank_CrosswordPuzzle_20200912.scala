import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.mutable.ArrayBuffer

/**
  * https://www.hackerrank.com/challenges/crossword-puzzle
  */
object HackerRank_CrosswordPuzzle_20200912 {

  // Complete the crosswordPuzzle function below.
  def crosswordPuzzle(crossword: Array[String], words: String): Array[String] = {

    case class WordLoc(start: (Int, Int), end: (Int, Int)) {
      val length: Int = end._1 - start._1 + end._2 - start._2 // one pair will be equal
      val horizontal: Boolean = start._1 == end._1
      def intersection(that: WordLoc): Option[(Int, Int)] = {
        //println(f"${this}.intersection(${that})")
        if (this.horizontal == that.horizontal)
          None
        else if (this.horizontal)
          that.intersection(this)
        else if (that.start._1 >= this.start._1 && that.end._1 < this.end._1 &&
          this.start._2 >= that.start._2 && this.end._2 < that.end._2)
          Some(that.start._1, this.start._2)
        else
          None
      }
      def indexOf(coord: (Int, Int)): Int =
        coord._1 - start._1 + coord._2 - start._2
    }

    case class Crossword(crossword: Array[String]) {
      def apply(i: Int, j: Int) = crossword(i).charAt(j)
    }

    val plus = scala.collection.immutable.Set('+', 'X')

    def identifyWordLocations(cw: Crossword): ArrayBuffer[WordLoc] = {
      val locs = ArrayBuffer[WordLoc]()
      for(i <- 0 until 10; j <- 0 until 10) cw(i, j) match {
        case x if plus contains x => ()
        case '-' => {
          if ((i == 0 || (plus contains cw(i-1, j))) && i != 9 && cw(i+1, j) == '-') {
            val iend = (i until 10).find(plus contains cw(_, j)).getOrElse(10)
            locs += WordLoc((i, j), (iend, j))
          }

          if ((j == 0 || (plus contains cw(i, j-1))) && j != 9 && cw(i, j+1) == '-') {
            val jend = (j until 10).find(plus contains cw(i, _)).getOrElse(10)
            locs += WordLoc((i, j), (i, jend))
          }
        }
      }
      locs
    }

    val cw = Crossword(crossword)
    val locs = identifyWordLocations(cw)
    println(f"locs = ${locs}")

    val intersections = (for(i <- 0 until locs.size; j<- i+1 until locs.size) yield
      locs(i).intersection(locs(j)).map(x => (locs(i), locs(j), x))).flatten
    println(f"intersections = ${intersections}")

    def fits(word: String, loc: WordLoc, partialSolution: scala.collection.immutable.Map[WordLoc, String]): Boolean = {
      if ((partialSolution contains loc) || loc.length != word.size)
        false
      else intersections.forall { case (from, to, crossingIdx) =>
        if (from == loc || to == loc) {
          val crossingLoc: WordLoc = if (from == loc) to else from
          !(partialSolution contains crossingLoc) || {
            val crossingWord = partialSolution(crossingLoc)
            val wchar = word.charAt(loc.indexOf(crossingIdx))
            val cchar = crossingWord.charAt(crossingLoc.indexOf(crossingIdx))
            wchar == cchar
          }
        } else true
      }
    }

    def solve(remainingWords: Seq[String], partialSolution: scala.collection.immutable.Map[WordLoc, String]): Option[scala.collection.immutable.Map[WordLoc, String]] = {

      println(f"solve(${remainingWords}, $partialSolution")

      if (remainingWords.isEmpty)
        Some(partialSolution)
      else {
        val word = remainingWords.head

        locs.filter(loc => fits(word, loc, partialSolution))
          .map(loc => solve(remainingWords.tail, partialSolution + (loc -> word)))
          .find(_.isDefined)
          .flatten
      }
    }

    // a solution is a map from word to WordLoc
    val solution: Option[scala.collection.immutable.Map[WordLoc, String]] = solve(words.split(";"), scala.collection.immutable.Map.empty[WordLoc, String])
    println(f"solution = ${solution}")


    solution.map { solutionMap =>

      for((loc, word) <- solutionMap) {

        var curr = loc.start
        while(curr != loc.end) {
          val i = loc.indexOf(curr)
          val row = crossword(curr._1)
          crossword(curr._1) = row.substring(0, curr._2) + word.charAt(i) + row.substring(curr._2 + 1, 10)
          curr = if (loc.horizontal) (curr._1, curr._2 + 1) else (curr._1 + 1, curr._2)

        }
      }

      crossword

    }.getOrElse(null)

  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val crossword = Array.ofDim[String](10)

    for (i <- 0 until 10) {
      val crosswordItem = stdin.readLine
      crossword(i) = crosswordItem}

    val words = stdin.readLine

    val result = crosswordPuzzle(crossword, words)

    printWriter.println(result.mkString("\n"))

    printWriter.close()
  }
}
