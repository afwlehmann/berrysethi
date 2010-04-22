package berrysethi


import scala.util.parsing.combinator.Parsers
import berrysethi._


class BRRegExParser extends Parsers {

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
   *    T -> (E) | (E|E) | Sigma
   */


  lazy val expr: Parser[BRTree] =
    multiplier ~ expr ^^ { case l ~ r => Concat(l, r) } |
    multiplier ^^ { case n: BRTree => n }


  lazy val multiplier: Parser[BRTree] =
    term <~ '*' ^^ { case t => Star(t) } |
    term <~ '?' ^^ { case t => QuestionMark(t) } |
    term


  lazy val term: Parser[BRTree] =
    '(' ~> expr <~ ')' |
    '(' ~> expr ~ '|' ~ expr <~ ')' ^^ { case l ~ '|' ~ r => Or(l, r) } |
    Sigma


  val Sigma: Parser[Leaf] =
    new Parser[Leaf] {
      def apply(in: Input) = {
        if (in.offset >= in.source.length)
          Failure("End of input", in.drop(in.offset))
        else {
          in.source.charAt(in.offset) match {
            case ch: Char if (!(reserved contains ch)) =>
              Success(Leaf(ch), in.rest)

            case _ =>
              Failure("Expected [^" + reserved + "], got '" + in.first + "' instead.",
                      in.drop(in.offset))
          }
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
          val aux = in.source.charAt(in.offset)
          if (aux == ch)
            Success(aux, in.rest)
          else
            Failure("Expected '" + ch + "', got '" + aux + "' instead.",
                    in.drop(in.offset))
        }
      }
    }

}
