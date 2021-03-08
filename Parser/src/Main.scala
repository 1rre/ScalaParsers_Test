
import util.parsing.input.CharSequenceReader

object Main extends App with parsing.C_Parser {
  
  // Make some test inputs
  val testInputs = Seq(
    // Sum of 2 ints (expected: 6)
    new CharSequenceReader("5+1"),
    // Sum of multiple ints (expected: 15)
    new CharSequenceReader("1+2+3+4+5"),
    // Sum of multiple ints with whitespace (expected: 15)
    new CharSequenceReader("""5 +
     4 + 3 + 2
      + 1"""),
    // Sum of multiple ints > 10 (expected: 150)
    new CharSequenceReader("1+2+10+20+35+35+47"),
    // Sum of ints with a space between 2 ints (expected: 12 & extra input after space)
    new CharSequenceReader("1+2+10 20+35+35+47"),
    // Single int (expected: no valid additions)
    new CharSequenceReader("3")
  )

  // A function to test a parser
  def test(parser: Parser[Addable]) = {
      // For each test input
      for (input <- testInputs) {
        // Parse the string & match the result
        parser(input) match {
          // If it is a success with no extra chars at the end
          case Success(result, reader) if reader.atEnd => {
            // Print the details about the list
            println(s"Parsed addition: $result")
            println(s"Sum is: ${result.toInt}")
          }
          // If it begins with a sum but has extra at the end
          case Success(result, reader) =>
            // Print the details about the list & then the unexpected input
            println(s"Parsed addition: $result")
            println(s"Sum is: ${result.toInt}")
            // Sorry about the nasty way of getting the extra input, it's not really meant to be printed
            println(s"Unexpected extra: ${reader.source.subSequence(reader.offset,reader.source.length)}")
          // Otherwise print that it didn't contain a valid addition
          case _ => println(s"${input.source} didn't contain a valid addition")
        }
        // Print the list & the sum of the list
        
      }
  }

  
  println("Using the recursive method:")
  test(addition)
  println
  println("Using the list method:")
  test(addition0)
  
}
