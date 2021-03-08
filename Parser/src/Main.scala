
import util.parsing.input.CharSequenceReader

object Main extends App with parsing.C_Parser {
  
  // Make some test inputs
  val testInputs = Seq(
    new CharSequenceReader("5+1"),
    new CharSequenceReader("1+2+3+4+5"),
    new CharSequenceReader("5 + 4 + 3 + 2 + 1")
  )

  println("Using the recursive method:")
  // For each test input
  for (input <- testInputs) {
    // Parse the string & get the result, or if it fails cause an error saying which input failed
    val parseResult = addition(input).getOrElse(sys.error(s"${input.source} didn't contain a valid sum."))
    // Print the list & the sum of the list
    println(s"Parsed list: $parseResult")
    println(s"Sum is: ${parseResult.toInt}")
  }

  println

  println("Using the list method:")
  for (input <- testInputs) {
    // Parse the string & get the result, or if it fails cause an error saying which input failed
    val parseResult = addition0(input).getOrElse(sys.error(s"${input.source} didn't contain a valid sum."))
    // Print the list & the sum of the list
    println(s"Parsed list: $parseResult")
    println(s"Sum is: ${parseResult.toInt}")
  }
  
}
