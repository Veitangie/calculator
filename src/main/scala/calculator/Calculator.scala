package calculator

import scala.annotation.tailrec
import calculator.CalculationError.*

type CalculationResult[A] = Either[CalculationError, A]

extension [A](v: A) def right: CalculationResult[A] = Right(v)

extension [E <: CalculationError](e: E) def left[A]: CalculationResult[A] = Left(e)

sealed abstract private class Calculator:
  def value: CalculationResult[Double]

  def append(that: CalculationResult[Calculator]): CalculationResult[Calculator]

  def append(that: Char): CalculationResult[Calculator]

  def push: Calculator = this

  def setAppendAfterPoint(): CalculationResult[Calculator]

sealed abstract private class Value extends Calculator

final private case class Number(content: Double, rDivisor: Int = 10, appendAfterPoint: Boolean = false) extends Value:
  override def value: CalculationResult[Double] = content.right

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] =
    that.map(r => Product(this, r))

  override def append(that: Char): CalculationResult[Calculator] =
    val (value, newDivisor) =
      if appendAfterPoint then (content + that.getNumericValue.doubleValue / rDivisor, rDivisor * 10)
      else (content * 10 + that.getNumericValue.doubleValue, rDivisor)
    Number(value, newDivisor, appendAfterPoint).right

  override def setAppendAfterPoint(): CalculationResult[Calculator] =
    if appendAfterPoint then IncorrectPointPlacement.left
    else this.copy(appendAfterPoint = true).right

private case object EmptyValue extends Value:
  override def value: CalculationResult[Double] = EmptyInput.left

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] = that

  override def append(that: Char): CalculationResult[Calculator] = Number(that.getNumericValue.doubleValue).right

  override def setAppendAfterPoint(): CalculationResult[Calculator] = Number(0d, appendAfterPoint = true).right

sealed abstract private class Operator(l: Calculator, r: Calculator) extends Calculator:
  def method: (Double, Double) => Double

  def rightValue: Calculator = r

  def predicate: (Double, Double) => Boolean = (_, _) => true

  def error: CalculationError = UnknownError

  def value: CalculationResult[Double] =
    for
      l   <- l.value
      r   <- r.value
      res <- Either.cond(predicate(l, r), method(l, r), error)
    yield res

  def copy(l: Calculator = l, r: Calculator = r): Operator

  def append(that: CalculationResult[Calculator]): CalculationResult[Operator] =
    for r <- r.append(that)
    yield copy(r = r)

  override def append(that: Char): CalculationResult[Calculator] =
    for r <- r.append(that)
    yield copy(r = r)

  override def setAppendAfterPoint(): CalculationResult[Calculator] =
    r.setAppendAfterPoint().map(newR => copy(r = newR))

  override def push: Operator = this match
    case Division(l, _)  => fall(l)
    case Product(l, _)   => fall(l)
    case Power(l, _)     => fall(l)
    case Sin(l, _)       => fall(l)
    case Cos(l, _)       => fall(l)
    case Factorial(_, r) => fall(r)
    case _               => this

  def fall(that: Calculator): Operator =
    that match
      case Addition(l, r)    => Addition(l, this.copy(l = r))
      case Subtraction(l, r) => Subtraction(l, this.copy(l = r))
      case _                 => this

final private case class Addition(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = _ + _

  override def copy(l: Calculator, r: Calculator): Operator = Addition(l, r)

final private case class Subtraction(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = _ - _

  override def copy(l: Calculator, r: Calculator): Operator =
    val newL =
      if l == EmptyValue then Number(0d)
      else l
    Subtraction(newL, r)

final private case class Division(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = _ / _

  override def predicate: (Double, Double) => Boolean = (_, r) => r != 0d

  override def error: CalculationError = DivisionByZero

  override def copy(l: Calculator, r: Calculator): Operator = Division(l, r)

final private case class Product(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = _ * _

  override def copy(l: Calculator, r: Calculator): Operator = Product(l, r)

final private case class Power(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = scala.math.pow

  override def copy(l: Calculator, r: Calculator): Operator = Power(l, r)

  override def fall(that: Calculator): Operator =
    that match
      case _: Value           => this
      case operator: Operator => operator.copy(r = this.copy(l = operator.rightValue, r = r))

final private case class Sin(l: Calculator = Number(0d), r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = (_, r) => scala.math.sin(r)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number(0d)
      case _: Calculator => l
    Sin(newL, r)

final private case class Cos(l: Calculator = Number(0d), r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = (_, r) => scala.math.cos(r)

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number(0d)
      case _: Calculator => l
    Cos(newL, r)

final private case class Log(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = (l, r) => scala.math.log(r) / scala.math.log(l)

  override def append(that: Char): CalculationResult[Calculator] = l.append(that).map(l => copy(l = l))

  override def append(that: CalculationResult[Calculator]): CalculationResult[Operator] =
    if l == EmptyValue then l.append(that).map(l => copy(l = l))
    else super.append(that)

  override def copy(l: Calculator = l, r: Calculator = r): Operator = Log(l, r)

  override def error: CalculationError = IllegalLogarithm

  override def predicate: (Double, Double) => Boolean = (l, r) => l > 0 && l != 1 && r > 0

final private case class Factorial(l: Calculator = Number(0d), r: Calculator) extends Operator(l, r):
  override def method: (Double, Double) => Double = (_, r) => factorial(r)

  @tailrec
  private def factorial(value: Double, acc: Double = 1d, current: Double = 0d): Double =
    if current == value then acc
    else factorial(value, acc * (current + 1), current + 1)

  override def predicate: (Double, Double) => Boolean = (_, r) => r.isWhole && r.isFinite && r >= 0

  override def error: CalculationError = IllegalFactorial

  override def copy(l: Calculator = l, r: Calculator = r): Operator =
    val newL = l match
      case EmptyValue    => Number(0d)
      case _: Calculator => l
    Factorial(newL, r)

  override def fall(that: Calculator): Operator =
    that match
      case _: Value           => this.copy(l = Number(0d), r = l)
      case operator: Operator => operator.copy(r = this.copy(l = r, r = operator.rightValue))

object Calculator:
  def calculate(source: String): CalculationResult[Double] =
    constructCalculator(
      formatSource(source).toList
    )
      .flatMap(_.value)

  private def formatSource(source: String): String =
    val constReg = """([ep])(\d)""".r
    val logReg   = """(l[ng])(\d+)""".r
    val minReg   = """([ng^)s/*])-(\d+|e|p)""".r
    val stripped = source.toLowerCase
      .replace(" ", "")
      .replace("pi", "p")
    val constFormatted = constReg.replaceAllIn(stripped, m => s"${m.group(1)}*${m.group(2)}")
    val logFormatted   = logReg.replaceAllIn(constFormatted, m => s"${m.group(1)}(${m.group(2)})")
    val minFormatted   = minReg.replaceAllIn(logFormatted, m => s"${m.group(1)}(-${m.group(2)})")
    minFormatted
      .replace("sin", "s")
      .replace("cos", "c")
      .replace("ln", "le")
      .replace("lg", "l10")
      .replace("log", "l")

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
            val res = for
              calc <- constructCalculator(acc.reverse)
              ans  <- calc.value
            yield Number(ans)
            (source.tail, res)
          else collectLayer(source.tail, source.head :: acc, currentDepth - 1)
        case _ => collectLayer(source.tail, source.head :: acc, currentDepth)

  @tailrec
  private def constructCalculator(
    source: List[Char],
    acc: CalculationResult[Calculator] = Right(EmptyValue)
  ): CalculationResult[Calculator] =
    if acc.isLeft || source.isEmpty then acc.map(_.push)
    else
      source.head match
        case c if c.isDigit =>
          constructCalculator(source.tail, acc.flatMap(_.append(source.head)))
        case '(' =>
          val (tail, layer) = collectLayer(source.tail)
          constructCalculator(tail, acc.flatMap(_.append(layer)))
        case ')' =>
          IncorrectParenthesesSequence.left
        case '.' =>
          constructCalculator(source.tail, acc.flatMap(_.setAppendAfterPoint()))
        case c =>
          constructCalculator(source.tail, acc.flatMap(res => getSymbol(c, res)))

  private def getSymbol(source: Char, l: Calculator): CalculationResult[Calculator] =
    source match
      case '+' => Addition(l.push, EmptyValue).right
      case '-' => Subtraction(l.push, EmptyValue).right
      case '/' => Division(l.push, EmptyValue).right
      case '*' => Product(l.push, EmptyValue).right
      case '^' => Power(l.push, EmptyValue).push.right
      case 's' => Sin(l.push, r = EmptyValue).right
      case 'c' => Cos(l.push, r = EmptyValue).right
      case 'p' => l.append(Number(scala.math.Pi).right)
      case 'e' => l.append(Number(scala.math.E).right)
      case '!' => Factorial(Number(0d), l.push).push.right
      case 'l' => l.append(Log(EmptyValue, EmptyValue).right)
      case _   => UnknownCharacter.left
