package berrysethi


// Not really necessary but helps in debugging :)
object NotImplementedException
  extends RuntimeException("Not yet implemented!")


class BRNFA(val root: BRTree) {

  /**
   * Whether the given node can be empty or not. Post-order traversal.
   * @param n           the node
   * @return            a Boolean indicating whether the node can be empty
   */
  def empty(n: BRTree): Boolean =
    n match {
      case Leaf(_) => false
      case Or(l, r) => empty(l) || empty(r)
      case Concat(l, r) => empty(l) && empty(r)
      case Asterisk(_) => true
      case QuestionMark(_) => true
      case _ => throw NotImplementedException
    }


  /**
   * The set of all first leaves for a given node. Post-order traversal.
   * @param n           the node
   * @return            a new instance of Set[Leaf]
   */
  def first(n: BRTree): Set[Leaf] =
    n match {
      case l: Leaf => Set(l)
      case Or(l, r) => first(l) ++ first(r)
      case Concat(l, r) => first(l) ++ (if (empty(l)) first(r) else Set.empty)
      case Asterisk(n) => first(n)
      case QuestionMark(n) => first(n)
      case _ => throw NotImplementedException
  }


  /**
   * The set of all last leaves for a given node. Post-order traversal.
   * @param n           the node
   * @return            a new instance of Set[Leaf]
   */
  def last(n: BRTree): Set[Leaf] =
    n match {
      case l: Leaf => Set(l)
      case Or(l, r) => last(l) ++ last(r)
      case Concat(l, r) => (if (empty(r)) last(l) else Set.empty) ++ last(r)
      case Asterisk(r1) => last(r1)
      case QuestionMark(r1) => last(r1)
      case _ => throw NotImplementedException
    }


  /**
   * The set of all next leaves for a given node. Pre-order traversal.
   * @param n           the node
   * @return            a new instance of Set[Leaf]
   */
  def next(n: BRTree): Set[Leaf] = {
    /**
     * Auxiliary function that returns a Set of the next leaves for a
     * particular node and whether that node has been found.
     * @param root      the current root node of the traversal
     * @param n         the node that we're looking for
     * @param acc       the Set of leaves that have been accumulated so far
     * @return          a tuple consisting of the Set of leaves and a boolean
     *                  indicating whether `n' has already been found
     *
     */
    def auxNext(root: BRTree, n: BRTree, acc: Set[Leaf]): (Set[Leaf], Boolean) =
      if (root == n)
        (acc, true)
      else
        root match {
          case Leaf(_) => (acc, false)
          case Or(l, r) => auxNext(l, n, acc) match {
              case result @ (_, true) => result
              case _ => auxNext(r, n, acc)
            }
          case Concat(l, r) => auxNext(r, n, acc) match {
              case result @ (_, true) => result
              case _ => auxNext(l, n, first(r) ++ (if (empty(r)) acc else Nil))
            }
          case Asterisk(r1) => auxNext(r1, n, first(r1) ++ acc)
          case QuestionMark(r1) => auxNext(r1, n, acc)
          case _ => throw NotImplementedException
        }

    // Start the traversal with an empty accumulator at root node.
    auxNext(root, n, Set.empty) match {
      case (acc, true) => acc
      case _ => throw new IllegalArgumentException("No such node.")
    }
  }


  /**
   * The set of all states of this NFA.
   */
  val states: Set[BRTree] = {
    /**
     * Auxiliary function to help us accumulate all the leaves.
     * @param root      the current root node of the traversal
     * @param acc       the set of leaves that have been accumulated so far
     * @return          the set of all leaves under the given root node
     */
    def leaves(root: BRTree, acc: Set[BRTree]): Set[BRTree] = root match {
        case l: Leaf => acc + l
        case Or(l, r) => leaves(l, leaves(r, acc))
        case Concat(l, r) => leaves(l, leaves(r, acc))
        case Asterisk(r1) => leaves(r1, acc)
        case QuestionMark(r1) => leaves(r1, acc)
        case _ => throw NotImplementedException
      }
      
    // Start the traversal with an empty accumulator at the root node. Also,
    // insert the root node into the resulting set.
    leaves(root, Set.empty) + root
  }


  // Unlike a List, a Set is not covariant in its type parameter. But since the
  // root node is typically not an instance of Leaf, we really need to do
  // something about the missing covariance, i.e. explicit conversion to a
  // Set[BRTree]. As a sidenote: Scala is able to infer the needed type
  // automatically so that, actually, calling .toSet would suffice.
  val acceptingStates: Set[BRTree] =
    if (empty(root))
      last(root).toSet[BRTree]
    else
      last(root).toSet[BRTree]


  val transitions: Set[(BRTree, Char, BRTree)] =
    (states.foldLeft[Set[(BRTree, Char, BRTree)]](Set.empty)((acc, node) =>
      node match {
        case l @ Leaf(v) if (first(root) contains l) => acc + Tuple3(root, v, l)
        case _ => acc
      })) ++
    ((states zip states).foldLeft[Set[(BRTree, Char, BRTree)]](Set.empty)((acc, nodes) =>
      nodes match {
        case (l1: Leaf, l2 @ Leaf(v)) if (next(l1) contains l2) => acc + Tuple3(l1, v, l2)
        case _ => acc
      }))

}