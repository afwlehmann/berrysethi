/**
 * DFA.scala
 * @author Alexander Lehmann <afwlehmann@googlemail.com>
 */


package berrysethi.automata


/**
 * A deterministic finite automaton.
 */
class DFA[T](val Q: Set[State],
             val Sigma: Set[T],
             val delta: Map[(State, T), State],
             val q: State,
             val F: Set[State]) {

  /**
   * Whether this automaton is in an accepting state.
   */
  def accept: Boolean =
    !failure && F(q)


  /**
   * Whether this automaton is in the Failure state.
   */
  def failure: Boolean =
    q == Failure


  /**
   * Processes the given input.
   * @param input       one input of type T
   * @return            a new DFA[T] that has processed the given input
   */
  def step(input: T): DFA[T] =
    if (!(Sigma(input) && (delta isDefinedAt (q, input))))
      new DFA[T](Q, Sigma, delta, Failure, F)
    else
      new DFA[T](Q, Sigma, delta, delta((q, input)), F)

}
