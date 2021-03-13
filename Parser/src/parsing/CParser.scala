package parsing

import util.parsing.combinator._
import collection.mutable.HashSet

trait CParser extends RegexParsers {
  override def skipWhitespace = true
  abstract trait Token[T] {
    def parser: Parser[T]
  }
  implicit def token2Parser[T](t: Token[T]): Parser[T] = t.parser
}

object CParser extends IdentParser {
  var enums: Parser[Ident] = failure("")
  var typedefs: Parser[Ident] = failure("")
}