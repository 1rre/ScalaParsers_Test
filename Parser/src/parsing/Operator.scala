package parsing
import util.parsing.combinator._
import language.postfixOps

trait OperatorParser extends CParser {
  abstract trait Operator
  abstract class SymbolOperator(s: Parser[_]) extends Operator with Token[Operator] {
    val parser = s ^^^ this
  }

  case object `[` extends SymbolOperator("[")
  case object `]` extends SymbolOperator("]")
  case object `(` extends SymbolOperator("(")
  case object `)` extends SymbolOperator(")")
  case object `,` extends SymbolOperator(",")
  case object `?` extends SymbolOperator("?")
  case object `:` extends SymbolOperator(":")
  case object `.` extends SymbolOperator(".")
  case object `->` extends SymbolOperator("->")
  case object `|` extends SymbolOperator("|")
  case object `||` extends SymbolOperator("||")
  case object `&&` extends SymbolOperator("&&")
  case object `&` extends SymbolOperator("&")
  case object Neg extends SymbolOperator("~")
  case object `!` extends SymbolOperator("!")
  case object `++` extends SymbolOperator("++")
  case object `--` extends SymbolOperator("--")
  case object `+` extends SymbolOperator("+")
  case object `-` extends SymbolOperator("-")
  case object `*` extends SymbolOperator("*")
  case object `/` extends SymbolOperator("/")
  case object `%` extends SymbolOperator("%")
  case object `<<` extends SymbolOperator("<<")
  case object `>>` extends SymbolOperator(">>")
  case object `<` extends SymbolOperator("<")
  case object `>` extends SymbolOperator(">")
  case object `<=` extends SymbolOperator("<=")
  case object `>=` extends SymbolOperator(">=")
  case object `==` extends SymbolOperator("==")
  case object `!=` extends SymbolOperator("!=")
  case object `=` extends SymbolOperator("=")
  case object `*=` extends SymbolOperator("*=")
  case object `/=` extends SymbolOperator("/=")
  case object `%=` extends SymbolOperator("%=")
  case object `+=` extends SymbolOperator("+=")
  case object `-=` extends SymbolOperator("-=")
  case object `<<=` extends SymbolOperator("<<=")
  case object `>>=` extends SymbolOperator(">>=")
  case object `&=` extends SymbolOperator("&=")
  case object `^=` extends SymbolOperator("^=")
  case object `|=` extends SymbolOperator("|=")

  case object UnaryOperator extends SymbolOperator(Seq[Parser[Operator]](`&`,Neg,`!`,`++`,`--`,`*`,`+`,`-`).reduce(_|||_))
  case object PostfixOperator extends Token[Operator] {
    val parser = `++` | `--`
  }
  case object MultiplicativeOperator extends SymbolOperator(Seq[Parser[Operator]](`*`,`/`,`%`).reduce(_|||_))
  case object AdditiveOperator extends SymbolOperator(Seq[Parser[Operator]](`+`,`-`).reduce(_|||_))
  case object ShiftOperator extends SymbolOperator(Seq[Parser[Operator]](`<<`,`>>`).reduce(_|||_))
  case object RelationalOperator extends SymbolOperator(Seq[Parser[Operator]](`<`,`>`,`>=`,`<=`).reduce(_|||_))
  case object EqualityOperator extends SymbolOperator(Seq[Parser[Operator]](`==`,`!=`).reduce(_|||_))
  case object AssignmentOperator extends SymbolOperator(Seq[Parser[Operator]](`=`,`*=`,`/=`,`%=`,`+=`,`-=`,`<<=`,`>>=`,`&=`,`^=`,`|=`).reduce(_|||_))
  
  case object SymbolOperator extends Token[Operator] {
    val parser = Seq[Parser[Operator]] (
      `[`,`]`,`(`,`)`,`,`,`?`,`:`,`.`,`->`,`|`,`||`,`&&`,
      UnaryOperator,PostfixOperator,MultiplicativeOperator,
      AdditiveOperator,RelationalOperator,EqualityOperator,AssignmentOperator
    ).reduce(_|||_)
  }

}