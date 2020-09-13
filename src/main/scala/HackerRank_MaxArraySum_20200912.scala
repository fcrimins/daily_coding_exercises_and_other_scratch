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

https://www.hackerrank.com/challenges/max-array-sum/problem

Given an array of integers, find the subset of non-adjacent elements with the maximum sum. Calculate the sum of that subset.

For example, given an array  we have the following possible subsets:

  Subset      Sum
[-2, 3, 5]   6
  [-2, 3]      1
  [-2, -4]    -6
  [-2, 5]      3
  [1, -4]     -3
  [1, 5]       6
  [3, 5]       8
Our maximum subset sum is .

  Function Description

Complete the  function in the editor below. It should return an integer representing the maximum subset sum for the given array.

  maxSubsetSum has the following parameter(s):

  arr: an array of integers
  Input Format

The first line contains an integer, .
The second line contains  space-separated integers .

Constraints

Output Format

Return the maximum sum described in the statement.

Sample Input 0

5
3 7 4 6 5
Sample Output 0

13
Explanation 0

Our possible subsets are  and . The largest subset sum is  from subset

Sample Input 1

5
2 1 5 8 4
Sample Output 1

11
Explanation 1

Our subsets are  and . The maximum subset sum is  from the first subset listed.

Sample Input 2

5
3 5 -7 8 10
Sample Output 2

15
Explanation 2
  */
object HackerRank_MaxArraySum_20200912 {

  // Complete the maxSubsetSum function below.
  def maxSubsetSum(arr: Array[Int]): Int = {

    case class WithWithoutLast(w: Int, wo: Int)

    val wwo: WithWithoutLast = arr.foldLeft(WithWithoutLast(0, 0)) { (prev, x) =>

      val nextWith = prev.wo + math.max(x, 0)
      val nextWithout = scala.math.max(prev.w, prev.wo)

      WithWithoutLast(nextWith, nextWithout)
    }

    scala.math.max(wwo.w, wwo.wo)
  }


  /*arr.size match {
      case 0 => 0
      case 1 => Math.max(arr.head, 0)
      case _ => Math.max(arr.head + maxSubsetSum(arr.tail.tail), // include first element
                    maxSubsetSum(arr.tail)) // skip first element
  }*/

  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val n = stdin.readLine.trim.toInt

    val arr = stdin.readLine.split(" ").map(_.trim.toInt)
    val res = maxSubsetSum(arr)

    printWriter.println(res)

    printWriter.close()
  }
}


