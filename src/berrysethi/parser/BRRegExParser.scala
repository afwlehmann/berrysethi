/**
 * BRRegExParser.scala
 * @author Alexander Lehmann <lehmanna@in.tum.de>
 */


package berrysethi.parser


import scala.util.parsing.combinator.Parsers
import berrysethi._


object BRRegExParser extends Parsers {

  type Elem = Char


  /**
   * This sequence defines the set of reserved characters which are then
   * used by the terminal parser Sigma.
   */
  val reserved = "()*?|"


  /*
   * A simple grammar for regular expressions:
   *
   *    E -> ME | M
   *    M -> T* | T? | T
   *    T -> (O) | Sigma
   *    O -> E|O | E
   */


  lazy val expr: Parser[BRTree] =
    multiplier ~ expr ^^ { case l ~ r => Concat(l, r) } |
    multiplier ^^ { case n: BRTree => n }


  lazy val multiplier: Parser[BRTree] =
    term <~ '*' ^^ { case t => Asterisk(t) } |
    term <~ '?' ^^ { case t => QuestionMark(t) } |
    term


  lazy val term: Parser[BRTree] =
    '(' ~> or <~ ')' |
    Sigma


  lazy val or: Parser[BRTree] =
    expr ~ '|' ~ or ^^ { case l ~ '|' ~ r => Or(l, r) } |
    expr


//  Alternative for parsers `term' and `or':
//  lazy val term: Parser[BRTree] =
//    '(' ~> or <~ ')' |
//    '(' ~> rep1sep(expr, '|') <~ ')' ^^ { _.reduceRight(Or(_,_)) } |
//    Sigma


  val Sigma: Parser[Leaf] =
    new Parser[Leaf] {
      def apply(in: Input) = {
        if (in.offset >= in.source.length)
          Failure("End of input", in.drop(in.offset))
        else {
          val ch = in.source.charAt(in.offset)
          if (!(reserved contains ch))
            Success(Leaf(ch), in.rest)
          else
            Failure("Expected [^" + reserved + "], got '" + ch + "' instead.",
                    in.drop(in.offset))
        }
      }
    }


  /**
   * Implicit conversion of a character to a parser that accepts that
   * character.
   * @param ch        the character
   * @return          a new instance of Parser[Char]
   */
  protected implicit def charToCharParser(ch: Char): Parser[Char] =
    new Parser[Char] {
      def apply(in: Input) = {
        if (in.offset >= in.source.length)
          Failure("End of input", in.drop(in.offset))
        else {
          val chIn = in.source.charAt(in.offset)
          if (chIn == ch)
            Success(chIn, in.rest)
          else
            Failure("Expected '" + ch + "', got '" + chIn + "' instead.",
                    in.drop(in.offset))
        }
      }
    }

}
