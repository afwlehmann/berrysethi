/**
 * State.scala
 * @author Alexander Lehmann <lehmanna@in.tum.de>
 */


package berrysethi.automata


class State(val index: Int) {

  override def toString: String =
    "State(" + index + ")"

  override def equals(that: Any): Boolean =
    eq(that.asInstanceOf[AnyRef])
  
}

object State {

  var lastIndex = -1


  def apply: State = { lastIndex += 1; new State(lastIndex) }

}


object Failure extends State(-1)
