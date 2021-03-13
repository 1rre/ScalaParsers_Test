package parsing
import util.parsing.combinator._
import token.Tokens
import language.postfixOps

trait IdentParser extends CParser {
  case class Ident(name: String) {
    val parser = name ^^^ this
  }
  case object Ident extends Token[Ident] {
    val parser = ("[a-zA-Z_][a-zA-Z_0-9]*"r) ^^ (Ident(_)) 
  }
}