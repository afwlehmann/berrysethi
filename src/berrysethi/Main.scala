package berrysethi


import java.io.StringReader
import scala.util.parsing.input._
import parser.BRRegExParser


object Main {

  def main(args: Array[String]): Unit = {
    val parser = new BRRegExParser
    parser.expr(StreamReader(new StringReader("(a|b)*a(a|b)"))) match {
      case parser.Success(root, _) =>
        val nfa = BRNFA.regExToNFA(root)
        Console println nfa.accepts("aaab")

      case parser.NoSuccess(msg, _) =>
        Console println msg
    }
  }

}
