
import util.parsing.input.CharSequenceReader

object Main extends App with parsing.ExpressionParser {
  val test = new CharSequenceReader("5.5")

  println(PostfixExpression.parser(test))

  
}
