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
  *

https://www.hackerrank.com/challenges/abbr/problem

https://www.geeksforgeeks.org/check-possible-transform-one-string-another/





You can perform the following operations on the string, :

Capitalize zero or more of 's lowercase letters.
Delete all of the remaining lowercase letters in .
Given two strings,  and , determine if it's possible to make  equal to  as described. If so, print YES on a new line. Otherwise, print NO.

For example, given a=AbcDE and b=ABDE, in  we can convert  and delete  to match . If  and , matching is not possible because letters may only be capitalized or discarded, not changed.

Function Description

Complete the function  in the editor below. It must return either  or .

abbreviation has the following parameter(s):

a: the string to modify
b: the string to match
Input Format

The first line contains a single integer , the number of queries.

Each of the next  pairs of lines is as follows:
- The first line of each query contains a single string, .
- The second line of each query contains a single string, .

Constraints

String  consists only of uppercase and lowercase English letters, ascii[A-Za-z].
String  consists only of uppercase English letters, ascii[A-Z].
Output Format

For each query, print YES on a new line if it's possible to make string  equal to string . Otherwise, print NO.

Sample Input

1
daBcd
ABC
Sample Output

YES
Explanation

image

We have  daBcd and  ABC. We perform the following operation:

Capitalize the letters a and c in  so that  dABCd.
Delete all the remaining lowercase letters in  so that  ABC.
Because we were able to successfully convert  to , we print YES on a new line.



  */
object HackerRank_Abbreviation_20200911 {

  // Complete the abbreviation function below.
  /*def abbreviation0(a: String, b: String): Boolean = (a.size, b.size) match {
      case (0, _) => b.isEmpty
      case (_, 0) => a.toLowerCase == a // a must all be lowercase
      case (_, _) if a.head >= 'A' && a.head <= 'Z' =>
          a.head == b.head && abbreviation0(a.tail, b.tail)
      case (_, _) =>
          abbreviation0(a.head.toUpper + a.tail, b) ||
              abbreviation0(a.tail, b)
  }*/

  def abbreviation0(a: String, b: String): Boolean = {

    val dp = Array.ofDim[Boolean](a.size + 1, b.size + 1)
    for(i <- 0 to a.size; j <- 0 to b.size)
      dp(i)(j) = false
    dp(0)(0) = true

    for(i <- 0 until a.size; j <- 0 to b.size) {

      if (dp(i)(j)) {

        // uppercase'ization or already uppercase and equal to b[i]
        if (j < b.size && a.charAt(i).toUpper == b.charAt(j))
          dp(i + 1)(j + 1) = true

        if (!a.charAt(i).isUpper) // deletion
          dp(i + 1)(j) = true
      }
    }

    dp(a.size)(b.size)
  }

  def abbreviation(a: String, b: String): String =
    if (abbreviation0(a, b)) "YES" else "NO"

  /*def abbreviation0(a: String, b: String): Boolean = {

      val lowerIndexes = a.zipWithIndex
                          .filter(tup => tup._1 == tup._1.toLower)
                          .map(_._2)
      println(f"lowerIndexes = ${lowerIndexes}")

      val nLower = lowerIndexes.size
      println(f"nLower = ${nLower}")

      def munge(onoff: Int): String = {

          var lower_i = 0

          val munged = a.flatMap {
              case ch if ch >= 'A' && ch <= 'Z' => Some(ch)
              case ch => {
                  val lower_i_bit = (onoff >> lower_i) & 1
                  lower_i += 1
                  if (lower_i_bit == 1) Some(ch.toUpper) else None
              }
          }.foldLeft("")((s, ch) => s + ch)

          println(f"munge(${onoff}) = ${munged}")
          munged
      }

      (0 until math.pow(2, nLower).toInt).exists(munge(_) == b)
  }*/

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val a = stdin.readLine

      val b = stdin.readLine

      val result = abbreviation(a, b)

      printWriter.println(result)
    }

    printWriter.close()
  }
}
