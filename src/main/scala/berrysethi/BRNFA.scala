/**
 * BRNFA.scala
 * @author Alexander Lehmann <afwlehmann@googlemail.com>
 */

package berrysethi

import automata.NFA

// Not really necessary but helps in debugging :)
object NotImplementedException
  extends RuntimeException("Not yet implemented!")

protected class BRNFA(val root: BRTree) {

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
     * Auxiliary function that returns a Set of the next leaves for a particular
     * node and whether that node has been found.
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
   * The set of all leaves.
   */
  val leaves: Set[Leaf] = {
    def auxLeaves(node: BRTree): Set[Leaf] =
      node match {
        case l: Leaf => Set(l)
        case Or(l, r) => auxLeaves(l) ++ auxLeaves(r)
        case Concat(l, r) => auxLeaves(l) ++ auxLeaves(r)
        case Asterisk(r1) => auxLeaves(r1)
        case QuestionMark(r1) => auxLeaves(r1)
        case _ => throw NotImplementedException
      }
    auxLeaves(root)
  }


  /**
   * The set of all states of the corresponding NFA.
   *
   * Note: Sets are not covariant in their type, hence the explicit conversion.
   */
  val states: Set[BRTree] =
    leaves.toSet[BRTree] + root

  /**
   * The set of all accepting states of the corresponding NFA.
   *
   * Note: Sets are not covariant in their type, hence the explicit conversion.
   */
  val acceptingStates: Set[BRTree] =
    if (empty(root))
      last(root).toSet[BRTree] + root
    else
      last(root).toSet[BRTree]

  /**
   * The transitions relation of the corresponding NFA.
   */
  val transitions: Set[(BRTree, Char, BRTree)] = {
    // First, determine the outgoing transitions of the root node.
    val fromRootToLeaves =
      (states.foldLeft[Set[(BRTree, Char, BRTree)]](Set.empty)((acc, node) =>
        node match {
          case l @ Leaf(v) if (first(root) contains l) =>
            acc + Tuple3(root, v, l) // Explicit Tuple3 to prevent implicit conversion
          case _ => acc
        }))
    // Next, determine the transitions from leaves to leaves.
    val fromLeavesToLeaves = {
      val leafXLeaf = for (l1 <- states; l2 <- states) yield (l1, l2)
      leafXLeaf.foldLeft[Set[(BRTree, Char, BRTree)]](Set.empty)((acc, nodes) =>
        nodes match {
          case (l1: Leaf, l2 @ Leaf(v)) if (next(l1) contains l2) =>
            acc + Tuple3(l1, v, l2) // Explicit Tuple3 to prevent implicit conversion
          case _ => acc
        })}
    // The result is simply the union of the former two sets.
    fromRootToLeaves ++ fromLeavesToLeaves
  }

}

object BRNFA {

  def apply(tree: BRTree): NFA[Char] = {
    val aux = new BRNFA(tree)

    // The states returned by BRNFA.states are really just BRTree nodes, which
    // is why we need to create corresponding automate.State instances.
    val Q = aux.states map { _ => new automata.State }
    val mapping = (aux.states zip Q).toMap

    // Now that we have that mapping, we can determine the set of accepting
    // states and all the rest.
    val F = aux.acceptingStates map (mapping(_))
    val Sigma = aux.leaves map { _.v }
    val q0 = mapping(aux.root)

    // The BRNFA class computes the transitions relations as a subset of
    // BRTree x Char x BRTree, however, the NFA class needs a mapping from
    // State to Char x State.
    type TrRel = Map[automata.State, Set[(Char, automata.State)]]
    val Delta: TrRel =
      aux.states.foldLeft[TrRel](Map.empty){(acc, node) =>
        // First, let's find the corresponding transitions. Also, since for all
        // these transitions the starting node will be the same, map the 3-tuples
        // from BRTree x Char x BRTree to 2-tuples from Char x State, leaving out
        // the former BRTree and mapping the latter to its corresponding State.
        val reducedTransitions: Set[(Char, automata.State)] =
          aux.transitions filter {
            case (from, _, _) if (from == node) => true
            case _ => false
          } map { case (_, v, toNode) => (v, mapping(toNode)) }
        // Augment the hitherto accumulated mapping, if applicable.
        if (!reducedTransitions.isEmpty)
          acc + (mapping(node) -> reducedTransitions)
        else
          acc
      }

    // Done.
    new NFA[Char](Q, Sigma, Delta, q0, F)
  }

}
