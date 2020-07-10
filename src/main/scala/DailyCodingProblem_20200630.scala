import scala.collection.mutable

/**
  * This problem was asked by Google.
  *
  * Given an array of integers and a number k, where 1 <= k <= length of the array, compute the maximum values of each
  * subarray of length k.
  *
  * For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: [10, 7, 8, 8], since:
  *
  * 10 = max(10, 5, 2)
  * 7 = max(5, 2, 7)
  * 8 = max(2, 7, 8)
  * 8 = max(7, 8, 7)
  *
  * Do this in O(n) time and O(k) space. You can modify the input array in-place and you do not need to store the
  * results. You can simply print them out as you compute them.
  */
object DailyCodingProblem_20200630 extends App {

  def kmaxes(k: Int, array: Seq[Int]): Seq[Int] = if (k == 1) array else {

    // double-ended queue of (max over last k, index of max)
    var deque: Seq[(Int, Int)] = List()

    val result: mutable.ListBuffer[Int] = mutable.ListBuffer()

    array.zipWithIndex.foreach { case (v: Int, i: Int) => {
      println(f"i = $i and v = $v")

      // remove first element of deque if it's outside current k-window
      if (deque.nonEmpty && i - deque.head._2 == k)
        deque = deque.tail
      println(f"  A deque = $deque")

      // drop all of those on the top of stack that are less than the current
      while (deque.nonEmpty && v >= deque.last._1)
        deque = deque.dropRight(1)

      // push current element onto the stack
      deque = deque :+ (v, i)
      println(f"  B deque = $deque")

      // if at least k elements have been then the bottom of the stack is the largest/max
      if (i >= k - 1)
        result += deque.head._1
      println(f"  C deque = $deque\n")
    }}

    result
  }

  val result = kmaxes(3, List(10, 5, 2, 7, 8, 7))
  println(f"result = $result\n\n")
  assert(result == List(10, 7, 8, 8))

  // second result/assert
  assert(kmaxes(3, List(5, 10, 2, 7, 8, 7)) == List(10, 10, 8, 8))
}
