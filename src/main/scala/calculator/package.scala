import calculator.CalculationError

import java.math.MathContext
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

package object calculator:

  private val threadPool = Executors.newFixedThreadPool(10)
  given customExecutionContext: ExecutionContextExecutor = ExecutionContext.fromExecutor(threadPool)

  type CalculationResult[+A] = Either[CalculationError, A]

  extension [A](v: A) def right: CalculationResult[A] = Right(v)

  extension [E <: CalculationError](e: E) def left[A]: CalculationResult[A] = Left(e)

  val E: String = "2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713821785251664274"
  val P: String = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"
  val context: MathContext = MathContext(100, java.math.RoundingMode.HALF_UP)
