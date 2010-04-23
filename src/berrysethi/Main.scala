package berrysethi


import java.io.StringReader
import scala.util.parsing.input._
import parser.BRRegExParser


object Main {

  def main(args: Array[String]): Unit = {
    val txt = "(a|b)*a(a|b)"
    val parser = new BRRegExParser
    parser.expr(StreamReader(new StringReader(txt))) match {
      case parser.Success(root, _) =>
        val nfa = new BRNFA(root)
        Console println "\nTree:"
        Console println root
        Console println "\nStates:"
        nfa.states map { Console println _ }
//        Console println nfa.debug
        Console println "\nTransitions:"
        nfa.transitions map { Console println _ }

      case parser.NoSuccess(msg, _) =>
        Console println msg
    }
  }

}
