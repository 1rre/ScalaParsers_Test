package parsing

import util.parsing.combinator._
import io.BufferedSource
import language.postfixOps

trait C_Parser extends RegexParsers {

  // Tell the parser that it should ignore any whitespace left over
  override def skipWhitespace = true

  // This abstract trait is what all tokens will inherit from so that we can return them from functions etc.
  abstract trait ParserToken

  // We can use this trait as a 'marker' for things which can be added together
  abstract trait Addable {
    override def toString: String
    // All "addables" will have a value, so we declare it here
    val toInt: Int
  }

  // This is what we'll use for things like '+' and 'int' as it's a single **object** for all tokens
  case object Plus extends ParserToken

  // This is what we'll use for things like literals and identifiers as they have a value which can change
  case class IntLiteral(value: Int) extends ParserToken with Addable {
    override def toString = value.toString
    val toInt = value
  }

  // This is the token for an addition
  // It is addable itself as you can add 5+3 and 1+2 together to give (5+3) + (1+2)
  case class Addition(a: Addable, b: Addable) extends ParserToken with Addable {
    override def toString = s"( ${a.toString} + ${b.toString} )"
    // The value is the sum of the 2 addables which comprise it
    val toInt = a.toInt + b.toInt
  }

  // Parse an int by using an extended regex to find sequences of 1 or more digits & returning an int literal token
  private def int: Parser[IntLiteral] = "[0-9]+".r ^^ (i => IntLiteral(i.toInt))
  
  // Parse a plus by matching the string "+" and then returning the Plus token we made earlier
  // We use .type here because `Plus` is not a type, it is an object
  private def plus: Parser[Plus.type] = "+" ^^ (_ => Plus)

  // Turn a sequence of ints and plusses into a list of ints to sum
  def addition: Parser[Addition] = {
    // Either an int or a "sum list" (called recursively) followed by a plus (the output of which we'll ignore) & then an int literal
    // Notice the curly brackets rather than parentheses here: That means we can use "case"
    (int <~ plus) ~ (addition ||| int) ^^ {
      // 'a' followed by 'b'
      // Hover over 'b' and look at the type:
      // It's that because we have an "or" as the 2nd token & therefore we know it's an item with at a minimum the traits that "addition" and "int" both have.
      // Because addable is in there, we can still use it to make an "addition", and we do that between 'a' and 'b'
      case a ~ b => Addition(a,b)
    }
  }

  // This is an alternate, more efficient way, however it's probably slightly harder to understand
  def addition0: Parser[Addition] = {
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