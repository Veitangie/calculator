sealed trait CalculationError(message: String)

object CalculationError:
  case object UnknownCharacter extends CalculationError("Unknown character in the input.")
  case object IncorrectParenthesesSequence extends CalculationError("Incorrect parentheses sequence.")
  case object IncorrectMethodSequence extends CalculationError("Incorrect method sequence.")
  case object EmptyInput extends CalculationError("Empty input.")
  case object DivisionByZero extends CalculationError("DivisionByZero")
  case object UnknownError extends CalculationError("Unknown error: true = false.")