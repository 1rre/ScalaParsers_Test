package parsing

import util.parsing.combinator._
import token.Tokens
import io.BufferedSource
import language.postfixOps

trait C_Parser extends RegexParsers {

  // Tell the parser that it should ignore any whitespace left over
  override def skipWhitespace = true

  // We can use this trait as a 'marker' for things which can be added together
  abstract trait Addable {
    // All "addables" will have an int value, so we state that here
    def toInt: Int
    // Override the "+" operator on 2 addables to add them together
    def +(b: Addable) = toInt + b.toInt
  }

  // This is what we'll use for things like '+' and 'int' as it's a single **object** for all tokens
  case object Plus extends Tokens

  // This is what we'll use for things like literals and identifiers as they have a value which can change
  case class IntLiteral(value: Int) extends Tokens with Addable {
    override def toString = value.toString
    val toInt = value
  }

  // This is the token for an addition
  // It is addable itself as you can add 5+3 and 1+2 together to give (5+3) + (1+2)
  case class Addition(a: Addable, b: Addable) extends Tokens with Addable {
    override def toString = s"( ${a.toString} + ${b.toString} )"
    // The value is the sum of the 2 addables which comprise it
    val toInt = a + b
  }

  // Parse an int by using an extended regex to find sequences of 1 or more digits & returning an int literal token
  private def int = "[0-9]+".r ^^ (i => IntLiteral(i.toInt))
  
  // Parse a plus by matching the string "+" and then returning the Plus token we made earlier
  private def plus = "+" ^^^ Plus

  // Turn a sequence of ints and plusses into a list of ints to sum
  // As it is recursive we have to explicitly set the return type
  // Unfortunately the result is grouped wrong, but I'm sure this is fixable?
  def addition: Parser[Addable] = {
    // An int followed by a plus (which we ignore), followed by either an addition or an int
    // Notice the curly brackets rather than parentheses here: That means we can use "case"
    (int <~ plus) ~ (addition ||| int) ^^ {
      // 'a' followed by 'b'
      // Hover over 'b' and look at the type:
      // It's that because we have an "or" as the 2nd token & therefore we know it's an item with at a minimum the traits that "addition" and "int" both have.
      // Because addable is in there, we can still use it to make an "addition", and we do that between 'a' and 'b'
      case a ~ b => Addition(a,b)
    }
  }

  // This is an alternate way which uses a list instead of recursion, however it's probably slightly harder to understand 
  def addition0 = {
    // 1 or more sequences of an int then a plus, then a final int
    (int <~ plus +) ~ int ^^ {
      // This time 'a' is a list of int literals and 'b' is an int literal.
      // This means we have to form an addition from a list.
      case a ~ b => {
        // We do this by "reducing" 'a' into a single addable by taking each item of the list & forming an addition between it and the last
        val reducedA = a.reduce[Addable](Addition(_,_))
        // We then take the reduced form of 'a' and perform an addition between it and 'b'
        Addition(reducedA, b)
      }
    }
  }
}