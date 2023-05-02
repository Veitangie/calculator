package calculator

import scala.annotation.tailrec
import calculator.CalculationError.*
import cats.data.EitherT
import cats.{Monad, Parallel}
import cats.syntax.parallel.*
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import ch.obermuhlner.math.big.BigDecimalMath.*

import java.math.MathContext
import java.math.BigDecimal.*
import scala.BigDecimal
import scala.collection.SetOps
import scala.concurrent.ExecutionContext
import scala.math.BigDecimal
import scala.math.BigDecimal.{javaBigDecimal2bigDecimal, RoundingMode}
import scala.util.Try

sealed abstract class Calculator:
  def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal]

  def append(that: CalculationResult[Calculator]): CalculationResult[Calculator]

  def append(that: Char): CalculationResult[Calculator]

  def push: CalculationResult[Calculator] = this.right

trait Operator(l: Calculator, r: Calculator) extends Calculator:
  def method: (BigDecimal, BigDecimal) => BigDecimal

  def rightOperand: Calculator = r

  def predicate: (BigDecimal, BigDecimal) => Boolean = (_, _) => true

  def error: CalculationError = UnknownError

  def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    //fixme: There certainly exists a better way to parallelize these computations. I just don't know it yet.
    val rVal = EitherT.liftF(().pure).flatMap(_ => r.value)
    val lVal = EitherT.liftF(().pure).flatMap(_ => l.value)
    (lVal, rVal).parFlatMapN { (lVal, rVal) =>
      for
        _   <- EitherT.cond(predicate(lVal, rVal), (), error)
        res <- EitherT.fromOption(Try(method(lVal, rVal)).toOption, FailedToProcess)
      yield res
    }

  def copy(l: Calculator = l, r: Calculator = r): Operator

  def append(that: CalculationResult[Calculator]): CalculationResult[Operator] =
    r.append(that).map(r => copy(r = r))

  override def append(that: Char): CalculationResult[Calculator] =
    r.append(that).map(r => copy(r = r))

trait Value extends Calculator

final case class OperatorConstructor(prev: Calculator, content: String) extends Calculator:
  override def append(that: Char): CalculationResult[Calculator] =
    if that.isDigit || that == '.' || that == 'p' || that == 'e' then getOperator.flatMap(_.append(that))
    else OperatorConstructor(prev = prev, content = content + that).right

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] =
    getOperator.flatMap(_.append(that))

  override def value[F[_]: Parallel: Monad](implicit ec: ExecutionContext): EitherT[F, CalculationError, BigDecimal] =
    EitherT.leftT(EmptyInput)

  private def construct(
    constructor: Calculator => Calculator
  ): CalculationResult[Calculator] =
    prev.push.map(constructor)

  private def getOperator: CalculationResult[Calculator] =
    content match
      case "s"   => construct(Sin(_, EmptyValue))
      case "sh"  => construct(Sinh(_, EmptyValue))
      case "c"   => construct(Cos(_, EmptyValue))
      case "ch"  => construct(Cosh(_, EmptyValue))
      case "ct"  => construct(Cot(_, EmptyValue))
      case "cth" => construct(Coth(_, EmptyValue))
      case "as"  => construct(Asin(_, EmptyValue))
      case "at"  => construct(Atan(_, EmptyValue))
      case "ac"  => construct(Acos(_, EmptyValue))
      case "act" => construct(Acot(_, EmptyValue))
      case "t"   => construct(Tan(_, EmptyValue))
      case "th"  => construct(Tanh(_, EmptyValue))
      case "^"   => construct(Power(_, EmptyValue))
      case "+"   => construct(Addition(_, EmptyValue))
      case "-"   => construct(Subtraction(_, EmptyValue))
      case "/"   => construct(Division(_, EmptyValue))
      case "*"   => construct(Product(_, EmptyValue))
      case "!"   => construct(Factorial(_, EmptyValue))
      case s"!$other" =>
        for
          factorial <- construct(Factorial(_, EmptyValue))
          res       <- factorial.push
        yield OperatorConstructor(res, other)
      case "l" => prev.append(Log(EmptyValue, EmptyValue).right)
      case ")" => IncorrectParenthesesSequence.left
      case _   => UnknownCharacter.left

  override def push: CalculationResult[Calculator] = getOperator.flatMap(_.push)

object Calculator:
  def calculate[F[_]: Parallel: Monad](source: String): F[String] =
    EitherT
      .fromEither(
        constructCalculator(
          formatSource(source).toList
        )
      )
      .flatMap(r => r.value[F])
      .value
      .map {
        case Left(value)  => value.toString
        case Right(value) => value.toString
      }

  def getStrRep(source: String): String =
    constructCalculator(
      formatSource(source).toList
    ) match
      case Right(value) => value.toString
      case Left(error)  => error.toString

  private def formatSource(source: String): String =
    val constReg = """([ep])(\d)""".r
    val posReg   = """(l[ng]|\))(\d*\.\d+|\d+)""".r
    val negReg   = """([ng^sc/*])-(\d*\.\d+|\d+|e|p)""".r
    val stripped = source.toLowerCase
      .replace(" ", "")
      .replace("pi", "p")
    val constFormatted = constReg.replaceAllIn(stripped, m => s"${m.group(1)}*${m.group(2)}")
    val logFormatted   = posReg.replaceAllIn(constFormatted, m => s"${m.group(1)}(${m.group(2)})")
    val minFormatted   = negReg.replaceAllIn(logFormatted, m => s"${m.group(1)}(-${m.group(2)})")
    minFormatted
      .replace("sin", "s")
      .replace("cos", "c")
      .replace("ln", "le")
      .replace("lg", "l10")
      .replace("log", "l")
      .replace("tan", "t")
      .replace("ctg", "ct")

  @tailrec
  private def collectLayer(
    source: List[Char],
    acc: List[Char] = List(),
    currentDepth: Int = 0
  ): (List[Char], CalculationResult[Calculator]) =
    if source.isEmpty then (Nil, IncorrectParenthesesSequence.left)
    else
      source.head match
        case '(' => collectLayer(source.tail, source.head :: acc, currentDepth + 1)
        case ')' =>
          if currentDepth == 0 then
            val res = constructCalculator(acc.reverse)
            (source.tail, res)
          else collectLayer(source.tail, source.head :: acc, currentDepth - 1)
        case _ => collectLayer(source.tail, source.head :: acc, currentDepth)

  @tailrec
  private def constructCalculator(
    source: List[Char],
    acc: CalculationResult[Calculator] = Right(EmptyValue)
  ): CalculationResult[Calculator] =
    if acc.isLeft || source.isEmpty then acc.flatMap(_.push)
    else
      source.head match
        case '(' =>
          val (tail, layer) = collectLayer(source.tail)
          constructCalculator(tail, acc.flatMap(_.append(layer.map(Parentheses.apply))))
        case c =>
          val newAcc = acc.flatMap(_ match
            case oc: OperatorConstructor => oc.append(c)
            case calc: Calculator if c.isDigit || c == '.' || c == 'e' || c == 'p' =>
              calc.append(c)
            case calc: Calculator => OperatorConstructor(calc, c.toString).right
          )
          constructCalculator(source.tail, newAcc)
