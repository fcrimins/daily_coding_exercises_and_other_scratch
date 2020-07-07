import scala.collection.mutable

/**
  * This problem was asked by Google.
  *
  * Implement locking in a binary tree. A binary tree node can be locked or unlocked only if all of its descendants or
  * ancestors are not locked.  [FWC - this probably isn't written exactly right... it's probably supposed to say
  * "descendants AND ancestors"]
  *
  * Design a binary tree node class with the following methods:
  *
  * * `is_locked`, which returns whether the node is locked
  * * `lock`, which attempts to lock the node. If it cannot be locked, then it should return false. Otherwise, it
  *   should lock it and return true.
  * * `unlock`, which unlocks the node. If it cannot be unlocked, then it should return false. Otherwise, it should
  *   unlock it and return true.
  *
  * You may augment the node to add parent pointers or any other property you would like. You may assume the class is
  * used in a single-threaded program, so there is no need for actual locks or mutexes. Each method should run in
  * O(h), where h is the height of the tree
  */
object DailyCodingProblem_20200706 extends App {

  case class Node(var value: String,
                  var parent: Option[Node] = None,
                  var left: Option[Node] = None,
                  var right: Option[Node] = None,
                  var lockedDescendants: mutable.Set[String] = mutable.Set[String](),
                  var isLocked: Boolean = false) {

    private def hasLockedAncestor: Boolean = parent.fold(false)(p => p.isLocked || p.hasLockedAncestor)

    private def hasLockedDescendant: Boolean = lockedDescendants.nonEmpty

    /** A binary tree node can be locked or unlocked only if all of its descendants or/AND? ancestors are not locked. */
    private def isLockable: Boolean = !hasLockedAncestor && !hasLockedDescendant

    private def addLockedDescendant(descendant: Node): Unit = {
      lockedDescendants += descendant.value
      this.parent.foreach(_.addLockedDescendant(descendant))
    }

    private def removeLockedDescendant(descendant: Node): Unit = {
      lockedDescendants -= descendant.value
      this.parent.foreach(_.addLockedDescendant(descendant))
    }

    def lock: Boolean = if (!isLockable) false else {
      this.isLocked = true
      this.parent.foreach(_.addLockedDescendant(this))
      true
    }

    def unlock: Boolean = if (!isLockable) false else {
      this.isLocked = false
      this.parent.foreach(_.removeLockedDescendant(this))
      true
    }

    override def toString: String = f"Node($value, $isLocked, ${lockedDescendants}, $isLockable)"
  }

  def createTree(value: String, numLevels: Int): Node = numLevels match {
    case 1 => {
      //print(f"  Creating node at level 1\n")
      Node(value)
    }
    case _ => {
      //print(f"Creating node at level $numLevels\n")
      val left = createTree(value + "L", numLevels - 1)
      val right = createTree(value + "R", numLevels - 1)
      val parent = Node(value, None, Some(left), Some(right))
      left.parent = Some(parent)
      right.parent = Some(parent)
      parent
    }
  }

  def traverseTree(node: Node, f: (Node, String) => Unit, spaces: String = ""): Unit = {
    f(node, spaces)
    node.left.foreach(n => traverseTree(n, f, spaces + "  "))
    node.right.foreach(n => traverseTree(n, f, spaces + "  "))
  }

  var root = createTree("", 4)

  print(f"First traversal\n")
  traverseTree(root, (n, spaces) => print(f"$spaces$n\n"))

  print(f"Lock LR = ${root.left.get.right.get.lock}\n")

  print(f"Second traversal\n")
  traverseTree(root, (n, spaces) => print(f"$spaces$n\n"))



}
