package parsing
import util.parsing.combinator._
import language.postfixOps


trait ExpressionParser extends IdentParser with ConstantParser with KeywordParser with OperatorParser {
  abstract trait Expression
  case object Expression extends Token[Expression] {
    val parser = failure("")
  }

  abstract trait PrimaryExpression extends Expression
  case object PrimaryExpression extends Token[Expression] {
    val parser = Ident^^(IdentExpression(_)) ||| Constant^^(ConstantExpression(_)) ||| StringLiteral^^(StringExpression(_)) ||| (`(` ~> Expression <~ `)`)
  }
  case class ConstantExpression[T](value: Constant[T]) extends PrimaryExpression {
    override def toString = value.toString
  }
  case class IdentExpression(value: Ident) extends PrimaryExpression {
    override def toString = value.toString
  }
  case class StringExpression(value: StringLiteral) extends PrimaryExpression {
    override def toString = value.toString
  }

  case class PostfixExpression(value: Expression, op: Operator) extends Expression
  case object PostfixExpression extends Token[Expression] {
    val parser = PrimaryExpression ~ (PostfixOperator ||| PFArray ||| PFArgs ||| PFMember ||| PFMemberPtr *) ^^ {
      case expression ~ operator => PostfixExpression(expression,operator.head)
    }
  }
  case class PFArray(value: Expression) extends Operator
  case object PFArray extends Token[PFArray] {
    val parser = `[` ~> Expression <~ `]` ^^ (PFArray(_))
  }
  case class PFArgs(value: Seq[Expression]) extends Operator
  case object PFArgs extends Token[PFArgs] {
    val parser = `[` ~> repsep(Expression,`,`) <~ `]` ^^ (PFArgs(_))
  }
  case class PFMember(value: Ident) extends Operator
  case object PFMember extends Token[PFMember] {
    val parser = `.` ~> Ident ^^ (PFMember(_))
  }
  case class PFMemberPtr(value: Ident) extends Operator
  case object PFMemberPtr extends Token[PFMemberPtr] {
    val parser = `->` ~> Ident ^^ (PFMemberPtr(_))
  }
}
