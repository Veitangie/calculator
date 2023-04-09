package calculator

sealed trait CalculationError(message: String):
  override def toString: String = message

object CalculationError:
  case object UnknownCharacter extends CalculationError("Unknown character in the input.")
  case object IncorrectParenthesesSequence extends CalculationError("Incorrect parentheses sequence.")
  case object IncorrectMethodSequence extends CalculationError("Incorrect method sequence.")
  case object EmptyInput extends CalculationError("Empty input.")
  case object DivisionByZero extends CalculationError("Division by zero.")
  case object UnknownError extends CalculationError("Unknown error: true = false.")
  case object IllegalFactorial extends CalculationError("Illegal factorial parameter.")
  case object IllegalLogarithm extends CalculationError("Illegal logarithm parameters.")