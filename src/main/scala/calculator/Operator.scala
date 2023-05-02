package calculator

import calculator.CalculationError.{
  DivisionByZero,
  IllegalAcos,
  IllegalAsin,
  IllegalCotangent,
  IllegalFactorial,
  IllegalLogarithm,
  IllegalTangent,
  IncorrectMethodSequence
}
import cats.{Monad, Parallel}
import cats.data.EitherT
import ch.obermuhlner.math.big.BigDecimalMath.{
  acos,
  acot,
  asin,
  atan,
  cos,
  cosh,
  cot,
  coth,
  log,
  pow,
  sin,
  sinh,
  tan,
  tanh
}
import cats.syntax.parallel.*
import cats.syntax.applicative.*
import cats.syntax.functor.*

import scala.annotation.tailrec
import scala.util.Try
import math.BigDecimal.javaBigDecimal2bigDecimal
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.language.postfixOps

final case class Addition(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = _ + _

  override def copy(l: Calculator, r: Calculator): Operator = Addition(l, r)

  override def toString: String = l.toString + "+" + r.toString

final case class Subtraction(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = _ - _

  override def copy(l: Calculator, r: Calculator): Operator = Subtraction(l, r)

  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    l match
      case EmptyValue =>
        for r <- r.value
        yield method(BigDecimal(0), r)
      case _ =>
        super.value

  override def toString: String = l.toString + "-" + r.toString

final case class Division(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = _ / _

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => r != BigDecimal(0)

  override def error: CalculationError = DivisionByZero

  override def copy(l: Calculator, r: Calculator): Operator = Division(l, r)

  override def toString: String = l.toString + "/" + r.toString

  override def push: CalculationResult[Calculator] = l match
    case Addition(l, r)         => Addition(l, Division(r, this.r)).right
    case Subtraction(l, r)      => Subtraction(l, Division(r, this.r)).right
    case _: OperatorConstructor => IncorrectMethodSequence.left
    case _                      => this.right

final case class Product(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = _ * _

  override def copy(l: Calculator, r: Calculator): Operator = Product(l, r)

  override def toString: String = l.toString + "*" + r.toString

  override def push: CalculationResult[Calculator] = l match
    case Addition(l, r)         => Addition(l, Product(r, this.r)).right
    case Subtraction(l, r)      => Subtraction(l, Product(r, this.r)).right
    case _: OperatorConstructor => IncorrectMethodSequence.left
    case _                      => this.right

final case class Power(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (l, r) => pow(l.bigDecimal, r.bigDecimal, context)

  override def copy(l: Calculator, r: Calculator): Operator = Power(l, r)

  override def toString: String = l.toString + "^" + r.toString

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      //fixme: For some reason scalac doesn't want to accept `this.copy(...)` due to some weird type conflicts.
      if op.rightOperand == EmptyValue then IncorrectMethodSequence.left
      else op.copy(r = Power(l = op.rightOperand, r = r)).right
    case _: OperatorConstructor => IncorrectMethodSequence.left
    case _                      => this.right

final case class Sin(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => sin(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Sin(newL, r)

  override def toString: String = "s" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Sinh(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => sinh(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Sinh(newL, r)

  override def toString: String = "sh" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Asin(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => asin(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Asin(newL, r)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => r >= -1 && r <= 1

  override def error: CalculationError = IllegalAsin

  override def toString: String = "as" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Cos(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => cos(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Cos(newL, r)

  override def toString: String = "c" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Cosh(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => cosh(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Cosh(newL, r)

  override def toString: String = "ch" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Acos(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => acos(r.bigDecimal, context)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => r >= -1 && r <= 1

  override def error: CalculationError = IllegalAcos

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Acos(newL, r)

  override def toString: String = "ac" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Tan(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => tan(r.bigDecimal, context)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => cos(r.bigDecimal, context) != 0

  override def error: CalculationError = IllegalTangent

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Tan(newL, r)

  override def toString: String = "t" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Tanh(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => tanh(r.bigDecimal, context)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => cos(r.bigDecimal, context) != 0

  override def error: CalculationError = IllegalTangent

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Tanh(newL, r)

  override def toString: String = "th" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Atan(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => atan(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Atan(newL, r)

  override def toString: String = "at" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Cot(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => cot(r.bigDecimal, context)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => sin(r.bigDecimal, context) != 0

  override def error: CalculationError = IllegalCotangent

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Cot(newL, r)

  override def toString: String = "ct" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Coth(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => coth(r.bigDecimal, context)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => sin(r.bigDecimal, context) != 0

  override def error: CalculationError = IllegalCotangent

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Coth(newL, r)

  override def toString: String = "cth" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Acot(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => acot(r.bigDecimal, context)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number("0")
      case _: Calculator => l
    Acot(newL, r)

  override def toString: String = "act" + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then op.copy(r = this.copy(l = EmptyValue)).right
      else IncorrectMethodSequence.left
    case _: Value               => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Log(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (l, r) =>
    log(r.bigDecimal, context) / log(l.bigDecimal, context)

  override def append(that: Char): CalculationResult[Calculator] = l.append(that).map(l => copy(l = l))

  override def append(that: CalculationResult[Calculator]): CalculationResult[Operator] =
    if l == EmptyValue then l.append(that).map(l => copy(l = l))
    else super.append(that)

  override def copy(l: Calculator = l, r: Calculator = r): Operator = Log(l, r)

  override def error: CalculationError = IllegalLogarithm

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (l, r) => l > 0 && l != 1 && r > 0

  override def toString: String = "l" + l.toString + "(" + r.toString + ")"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator => if op.rightOperand == EmptyValue then op.copy(r = this).right else IncorrectMethodSequence.left
    case _: Value     => this.right
    case _: OperatorConstructor => IncorrectMethodSequence.left

final case class Factorial(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (BigDecimal, BigDecimal) => BigDecimal = (_, r) => factorial(r)

  private def parallelFactorial[F[_]: Parallel: Monad](value: BigDecimal): F[BigDecimal] =
    val chunks     = value.toString().length.min(Runtime.getRuntime.availableProcessors())
    val (res, rem) = value /% BigDecimal(chunks)
    (0 until chunks)
      .map(i =>
        if i == chunks - 1 then (res * i + 1, res * (i + 1) + rem)
        else (res * i + 1, res * (i + 1))
      )
      .toVector
      .parTraverse { case (start, end) =>
        //fixme: There certainly exists a better way to parallelize these computations. I just don't know it yet.
        ().pure.map(_ => factorial(end, start, start))
      }
      .map(_.foldLeft(BigDecimal(1))(_ * _))

  @tailrec
  def factorial(value: BigDecimal, acc: BigDecimal = BigDecimal(1), current: BigDecimal = BigDecimal(1)): BigDecimal =
    if current == value || value == 0 then acc
    else factorial(value, acc * (current + 1), current + 1)

  override def predicate: (BigDecimal, BigDecimal) => Boolean = (_, r) => r.isWhole && r >= 0 && r <= 268500000

  override def error: CalculationError = IllegalFactorial

  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    for
      r   <- r.value
      _   <- EitherT.cond(predicate(BigDecimal(0), r), (), error)
      res <- EitherT.liftF(parallelFactorial(r))
    yield res

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    Factorial(l, r)

  override def toString: String = r.toString + "!"

  override def push: CalculationResult[Calculator] = l match
    case op: Operator =>
      if op.rightOperand == EmptyValue then IncorrectMethodSequence.left
      else op.copy(r = this.copy(l = EmptyValue, r = op.rightOperand)).right
    case EmptyValue             => this.right
    case _: Value               => this.copy(l = EmptyValue, r = l).right
    case _: OperatorConstructor => IncorrectMethodSequence.left
