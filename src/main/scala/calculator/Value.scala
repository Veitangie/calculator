package calculator

import calculator.CalculationError.EmptyInput
import cats.{Monad, Parallel}
import cats.data.EitherT

import scala.concurrent.ExecutionContext

final case class Number(content: String) extends Value:
  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    if content.isBlank then EitherT.fromEither(EmptyInput.left)
    else EitherT.fromEither(java.math.BigDecimal(content).right)

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] =
    that.map(r => Product(this, r))

  override def append(that: Char): CalculationResult[Calculator] = Number(content + that).right

  override def toString: String = content

final case class Parentheses(content: Calculator) extends Value:
  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    content.value

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] =
    that.map(r => Product(this, r))

  override def append(that: Char): CalculationResult[Calculator] = EmptyValue.append(that).map(r => Product(this, r))

  override def toString: String = "(" + content.toString + ")"

case object EmptyValue extends Value:
  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    EitherT.fromEither(EmptyInput.left)

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] = that

  override def append(that: Char): CalculationResult[Calculator] =
    val string = that match
      case '.' => "0" + '.'
      case 'p' => P
      case 'e' => E
      case c   => c.toString
    Number(string).right

  override def toString: String = "Empty"
