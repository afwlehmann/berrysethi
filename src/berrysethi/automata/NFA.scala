package berrysethi.automata


import berrysethi.NotImplementedException


class NFA[T](val Q: Set[State],
             val Sigma: Set[T],
             val Delta: Map[State, Set[(T, State)]],
             val q0: State,
             val F: Set[State]) {

  /**
   * Checks whether a sequence of transitions exists such that all of the
   * given input is consumed and all the same an accepting state is reached.
   * @param input         the remaining input
   * @param q             the current state
   * @return              whether the given input can be accepted
   */
  protected def auxAccepts(input: Iterable[T], q: State): Boolean =
    (input.isEmpty, F(q)) match {
      case (true, true) =>
        // All input was consumed and the current state is an accepting state.
        true

      case (false, _) =>
        // There is still more input waiting to be processed.
        if (!(Sigma(input.head) && (Delta isDefinedAt q))) {
          // Either the input is invalid or we're stuck in the current state.
          false
        } else {
          val possibleTransitions = Delta(q) filter ( _._1 == input )
          (false /: possibleTransitions)((acc, transition) =>
            if (acc) acc else auxAccepts(input.tail, transition._2))
        }

      case _ =>
        // The input is empty, but we're not in an accepting state.
        false
    }


  /**
   * Whether this automaton accepts the given input.
   * @param input         an input sequence
   * @return              whether the given input can be accepted
   */
  def accepts(input: Iterable[T]): Boolean =
    auxAccepts(input, q0)


  /**
   * TODO: Conversion of this NFA to an equivalent DFA.
   */
  def toDFA: DFA[T] =
    throw NotImplementedException
  
}
