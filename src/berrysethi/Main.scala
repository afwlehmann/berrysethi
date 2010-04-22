package berrysethi


import java.io.StringReader
import scala.util.parsing.input._


object Main {

  def main(args: Array[String]): Unit = {
    val txt = "(a|b)*a(a|b)"
    val parser = new BRRegExParser
    parser.expr(StreamReader(new StringReader(txt))) match {
      case parser.Success(root, _) =>
        val nfa = new BRNFA(root)
        Console println root
        Console println nfa.states
        Console println nfa.transitions
      case parser.NoSuccess(msg, _) =>
        Console println msg
    }
  }

}
