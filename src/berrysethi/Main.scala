/**
 * Main.scala
 * @author Alexander Lehmann <lehmanna@in.tum.de>
 */


package berrysethi


import java.io.StringReader
import scala.util.parsing.input._
import parser.BRRegExParser


object Main {

  def main(args: Array[String]): Unit = {
    if (args.length != 2 || args(0).isEmpty || args(1).isEmpty) {
      Console.err println "Syntax: java -jar berrysethi.jar <regex> <string>"
      System.exit(1)
    }

    val parser = new BRRegExParser
    parser.expr(StreamReader(new StringReader(args(0)))) match {
      case parser.Success(root, _) =>
        val nfa = BRNFA.regExToNFA(root)
        Console println nfa.accepts(args(1))

      case parser.NoSuccess(msg, _) =>
        Console println msg
    }
  }

}
