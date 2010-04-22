package berrysethi


import java.io.StringReader
import scala.util.parsing.input._


object Main {

  /**
   * @param args the command line arguments
   */
  def main(args: Array[String]): Unit = {
    val parser = new BRRegExParser
    val txt = "a(bc|d|i|g)*f?(g)"
    val foo = parser.expr(StreamReader(new StringReader(txt)))
    Console println foo
  }

}
