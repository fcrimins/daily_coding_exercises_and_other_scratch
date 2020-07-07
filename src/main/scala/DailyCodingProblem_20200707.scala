


/**
  * This problem was asked by Facebook.
  *
  * Implement regular expression matching with the following special characters:
  *
  * `.` (period) which matches any single character
  * `*` (asterisk) which matches zero or more of the preceding element
  * That is, implement a function that takes in a string and a valid regular expression and returns whether or not the
  * string matches the regular expression.
  *
  * For example, given the regular expression "ra." and the string "ray", your function should return true. The same
  * regular expression on the string "raymond" should return false.
  *
  * Given the regular expression ".*at" and the string "chat", your function should return true. The same regular
  * expression on the string "chats" should return false.
  */
object DailyCodingProblem_20200707 extends App {

  val ANY: Char = '.'
  val ZERO_OR_MORE: Char = '*'

  def matches(rgx: String, str: String): Boolean = rgx.length match {
    case 0 => str.isEmpty  // rgx is empty and str is empty, so they match
    case 1 => str.length == 1 && List(ANY, str.head).contains(rgx.head)  // no possibility for * following first char
    case _ => rgx.tail.head match {  // match on second char
      case ZERO_OR_MORE => matches(rgx.tail.tail, str) ||  // zero
                           str.nonEmpty && List(ANY, str.head).contains(rgx.head) && matches(rgx, str.tail)  // more
      case _ => str.nonEmpty && List(ANY, str.head).contains(rgx.head) && matches(rgx.tail, str.tail)  // exactly 1
    }
  }

  assert(matches("ra.", "ray"))
  assert(!matches("ra.", "raymond"))
  assert(matches(".*at", "chat"))
  assert(!matches(".*at", "chats"))
}
