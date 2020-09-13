import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

/**
  * https://www.hackerrank.com/challenges/ctci-recursive-staircase
  */
object HackerRank_RecursionDavisStaircase_20200912 {

  val cache = scala.collection.mutable.Map[Int, Int]()

  // Complete the stepPerms function below.
  def stepPerms(n: Int): Int = n match {
    case _ if n == 0 => 0
    case _ if n == 1 => 1
    case _ if n == 2 => 2
    case _ if n == 3 => 4
    case _ if cache.contains(n) => cache.get(n).get
    case _ =>
      val nMinus3 = stepPerms(n - 3)
      val nMinus2 = stepPerms(n - 2)
      val nMinus1 = stepPerms(n - 1)
      val retVal = ((nMinus1 + nMinus2 + nMinus3) % 10000000007L).toInt
      cache.put(n, retVal)
      retVal
  }

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val s = stdin.readLine.trim.toInt

    for (sItr <- 1 to s) {
      val n = stdin.readLine.trim.toInt

      val res = stepPerms(n)

      printWriter.println(res)
    }

    printWriter.close()
  }
}

