import scala.collection.mutable.MutableList

/**
  * This problem was asked by Google.
  *
  * Given k sorted singly linked lists, write a function to merge all the lists into one sorted singly linked list.
  */
object DailyCodingProblem_20200829 extends App {

  // k = 3 lists in this test
  val list0 = List(2, 5, 9)
  val list1 = List(1, 7, 8)
  val list2 = List(3, 4, 6, 63)
  val lists = Array(list0, list1, list2)

  // keep an iterator to the current element of each list along with the current element's value
  case class IteratorAndNext(var iter: Iterator[Int], var mbNext: Option[Int])
  val iterators = lists.map(lst => {
    val iter = lst.iterator
    val mbNext = if (iter.hasNext) Some(iter.next) else None
    IteratorAndNext(iter, mbNext)
  })

  // result: joined lists
  val joined = MutableList.empty[Int]

  // Scala doesn't have break/continue
  var continue = true
  while(continue) {
    var argmin: Option[IteratorAndNext] = None

    // for each current element of each list, figure out which is the min
    iterators.foreach( pair => {
      if (pair.mbNext.isDefined && (argmin.isEmpty || argmin.get.mbNext.get > pair.mbNext.get))
        argmin = Some(pair)
    })

    // isEmpty indicates that there are no more elements in any of the lists
    if (argmin.isEmpty) { continue = false } else {

      // append to joined lists
      joined += argmin.get.mbNext.get

      // update current element iterator and value, if there are any more for this list
      argmin.get.mbNext = if (argmin.get.iter.hasNext) Some(argmin.get.iter.next) else None
    }
  }

  val expected = MutableList(1, 2, 3, 4, 5, 6, 7, 8, 9, 63)
  println(f"joined.toString = ${joined.toString}")
  println(f"expected.toString = ${expected.toString}")
  assert(joined.toString == expected.toString)
}
