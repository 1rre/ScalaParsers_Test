package parsing
import util.parsing.combinator._
import token.Tokens
import language.postfixOps

trait ConstantParser extends IdentParser {
  private def simpleEscape = '\\' ~> ('a'^^^0x7l|'b'^^^0x8l|'e'^^^0x9l|'f'^^^0xcl|'n'^^^0xal|'r'^^^0xdl|'t'^^^0x9l|'v'^^^0xbl|'\\'^^^0x5cl|'\''^^^0x27l|'"'^^^0x22l|'?'^^^0x3fl)
  private def octEscape = '\\' ~> repNM(1,3,"[0-7]"r)^^(_.foldLeft(0l)(_*8+_.toLong))
  private def hexEscape = "\\x" ~> ("[0-9a-fA-F]+"r)^^(java.lang.Long.parseLong(_))
  private def escape = simpleEscape | octEscape | hexEscape

  case class StringLiteral(value: String)
  case object StringLiteral extends Token[StringLiteral] {
    private def simpleStr = """[^"\\\n]""".r ^^ (_.charAt(0).toLong)
    val parser = '"' ~> ((escape|||simpleStr) ^^ (_.toChar) +) <~'"' ^^ (_.mkString) ^^ (StringLiteral(_))
  }

  abstract class Constant[T] {
    def value: T
  }
  abstract class Number[T <: AnyVal] extends Constant[T]

  abstract class IntLiteral(value: Long) extends Number[Long]
  case class SInt(value: Long) extends IntLiteral(value)
  case class UInt(value: Long) extends IntLiteral(value)

  case object CChar extends Token[IntLiteral] {
    private def simpleChar = """[^'\\\n]""".r ^^ (_.charAt(0).toLong)
    val parser = '\'' ~> (simpleChar | escape +) <~ '\'' ^^ (_.reduce(_*256+_)) ^^ (SInt(_))
  }

  case object IntLiteral extends Token[IntLiteral] {
    val parser = (("[0-7]+"r) ||| ("[1-9]([0-9]*)"r) ||| ("0x[0-9a-zA-Z]+"r)) ~ ("u"|"U"?) <~ ("l"|"L"?) ^^ {
      case n ~ Some(_) => SInt(java.lang.Long.parseLong(n))
      case n ~ _ => UInt(java.lang.Long.parseLong(n).toLong)
    }
  }

  case class FloatLiteral(value: Double) extends Number[Double]
  
  case object FloatLiteral extends Token[FloatLiteral] {
    val parser = ("""[0-9]+\.[0.9]+"""r) ^^ (_.toDouble) ^^ (FloatLiteral(_))
  }

  case class EnumConstant(value: Ident) extends Constant[Ident]
  case object EnumConstant extends Token[EnumConstant] {
    val parser = CParser.enums.asInstanceOf[Parser[Ident]] ^^ (EnumConstant(_))
  }


  case object Constant extends Token[Constant[_]] {
    val parser = IntLiteral ||| FloatLiteral ||| CChar ||| EnumConstant
  }

}