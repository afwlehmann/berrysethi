/**
 * Main.scala
 * @author Alexander Lehmann <afwlehmann@googlemail.com>
 */

package berrysethi

import parser.BRRegExParser
import scala.util.parsing.input.StreamReader
import java.io.StringReader

object Main {

  def main(args: Array[String]): Unit = {
    if (args.length != 2 || args(0).isEmpty || args(1).isEmpty) {
      Console.err println "Syntax: java -jar berrysethi.jar <regex> <string>"
      System.exit(1)
    }

    BRRegExParser.expr(StreamReader(new StringReader(args(0)))) match {
      case BRRegExParser.Success(root, _) =>
        Console println BRNFA(root).accepts(args(1))
      case BRRegExParser.NoSuccess(msg, _) =>
        Console println msg
    }
  }

}
