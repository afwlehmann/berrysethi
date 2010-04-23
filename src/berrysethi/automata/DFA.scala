package berrysethi.automata


class DFA[T](val Q: Set[State],
             val Sigma: Set[T],
             val delta: Map[(State, T), State],
             val S: State,
             val F: Set[State]) {

  val q: State = S


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
    if (!(Sigma(input) && (delta isDefinedAt (S, input))))
      new DFA[T](Q, Sigma, delta, Failure, F)
    else
      new DFA[T](Q, Sigma, delta, delta((S, input)), F)

}
