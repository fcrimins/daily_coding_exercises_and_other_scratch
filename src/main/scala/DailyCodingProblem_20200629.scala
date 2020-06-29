import scala.collection.mutable.Stack

/**
  * This problem was asked by Google.
  *
  * Suppose we represent our file system by a string in the following manner:
  *
  * The string "dir\n\tsubdir1\n\tsubdir2\n\t\tfile.ext" represents:
  *
  * dir
  *     subdir1
  *     subdir2
  *         file.ext
  *
  * The directory dir contains an empty sub-directory subdir1 and a sub-directory subdir2 containing a
  * file file.ext.
  *
  * The string "dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext"
  * represents:
  *
  * dir
  *     subdir1
  *         file1.ext
  *         subsubdir1
  *     subdir2
  *         subsubdir2
  *         file2.ext
  *
  * The directory dir contains two sub-directories subdir1 and subdir2. subdir1 contains a file file1.ext
  * and an empty second-level sub-directory subsubdir1. subdir2 contains a second-level sub-directory
  * subsubdir2 containing a file file2.ext.
  *
  * We are interested in finding the longest (number of characters) absolute path to a file within our
  * file system. For example, in the second example above, the longest absolute path is
  * "dir/subdir2/subsubdir2/file2.ext", and its length is 32 (not including the double quotes).
  *
  * Given a string representing the file system in the above format, return the length of the longest
  * absolute path to a file in the abstracted file system. If there is no file in the system, return 0.
  */
object DailyCodingProblem_20200629 extends App {

  def f(str: String): Int = {

    var stack = Stack[String]()
    var longest = 0

    str.split('\n').foreach(elem => {
      val es = elem.split('\t')
      while(stack.size > es.length - 1) stack.pop  // pop to correct directory level

      if (es.last contains "dir") stack.push(es.last)
      else {
        val len = stack.map(_.length).sum + stack.size + es.last.length
        if (len > longest)
          longest = len
      }
    })

    longest
  }

  val ans = f("dir\n\tsubdir1\n\t\tfile1.ext\n\t\tsubsubdir1\n\tsubdir2\n\t\tsubsubdir2\n\t\t\tfile2.ext")
  assert(ans == 32)
  if (ans != 32)
    throw new Exception(s"Wrong answer; ${ans} != 32")
}
