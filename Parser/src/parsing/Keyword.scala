package parsing
import util.parsing.combinator._
import language.postfixOps

trait KeywordParser extends CParser {
  abstract class Keyword[T](s: Parser[_]) extends Token[T] {
    val parser = s ^^^ this.asInstanceOf[T]
  }
  case object Auto extends Keyword("auto")
  case object Double extends Keyword("double")
  case object Int extends Keyword("int")
  case object Struct extends Keyword("struct")
  case object Break extends Keyword("break")
  case object Else extends Keyword("else")
  case object Long extends Keyword("long")
  case object Switch extends Keyword("switch")
  case object Case extends Keyword("case")
  case object Enum extends Keyword("enum")
  case object Register extends Keyword("register")
  case object Typedef extends Keyword("typedef")
  case object Char extends Keyword("char")
  case object Extern extends Keyword("extern")
  case object Return extends Keyword("return")
  case object Union extends Keyword("union")
  case object Const extends Keyword("const")
  case object Float extends Keyword("float")
  case object Short extends Keyword("short")
  case object Unsigned extends Keyword("unsigned")
  case object Continue extends Keyword("continue")
  case object For extends Keyword("for")
  case object Signed extends Keyword("signed")
  case object Void extends Keyword("void")
  case object Default extends Keyword("default")
  case object Goto extends Keyword("goto")
  case object Sizeof extends Keyword("sizeof")
  case object Volatile extends Keyword("volatile")
  case object Do extends Keyword("do")
  case object If extends Keyword("if")
  case object Static extends Keyword("static")
  case object While extends Keyword("while")

}