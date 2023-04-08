import CalculationError.{DivisionByZero, EmptyInput, IncorrectMethodSequence, IncorrectParenthesesSequence, UnknownError, UnknownCharacter}

import scala.annotation.{tailrec, targetName}

type CalculationResult[A] = Either[CalculationError, A]

extension[A](v: A)
  def right: CalculationResult[A] = Right(v)

extension[E <: CalculationError, A](e: E)
  def left: CalculationResult[A] = Left(e)

sealed abstract class Calculator:
  def value: CalculationResult[Int]

  def append(that: CalculationResult[Calculator]):CalculationResult[Calculator]

  def push: Calculator = this

sealed abstract class Value extends Calculator

final case class Number(content: Int) extends Value:
  override def value: CalculationResult[Int] = content.right

  override def append(that: CalculationResult[Calculator]): CalculationResult[Value] =
    for
      that <- that
      newDigit <- that.value
      res <- Either.cond(that.isInstanceOf[Value], Number(content * 10 + newDigit), IncorrectMethodSequence)
    yield res

case object EmptyValue extends Value:
  override def value: CalculationResult[Int] = EmptyInput.left

  override def append(that: CalculationResult[Calculator]): CalculationResult[Calculator] = that

sealed abstract class Operator(l: Calculator, r: Calculator) extends Calculator:
  def method: (Int, Int) => Int

  def predicate: (Int, Int) => Boolean = (_, _) => true

  def error: CalculationError = UnknownError

  def value: CalculationResult[Int] =
    for
      l <- l.value
      r <- r.value
      res <- Either.cond(predicate(l, r),  method(l, r), error)
    yield
      res

  def copy(l: Calculator = l, r: Calculator = r): Operator

  def append(that: CalculationResult[Calculator]): CalculationResult[Operator] =
    for
      r <- r.append(that)
    yield
      copy(r = r)

  override def push: Operator = this match
    case Division(l, _) => fall(l)
    case Product(l, _) => fall(l)
    case _ => this

  private def fall(that: Calculator): Operator =
    that match
      case Addition(l, r) => Addition(l, this.copy(l = r))
      case Subtraction(l, r) => Subtraction(l, this.copy(l = r))
      case _ => this

final case class Addition(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Int, Int) => Int = _ + _

  override def copy(l: Calculator, r: Calculator): Operator = Addition(l, r)

final case class Subtraction(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Int, Int) => Int = _ - _

  override def copy(l: Calculator, r: Calculator): Operator = Subtraction(l, r)

final case class Division(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Int, Int) => Int = _ / _

  override def predicate: (Int, Int) => Boolean = (_, r) => r != 0

  override def error: CalculationError = DivisionByZero

  override def copy(l: Calculator, r: Calculator): Operator = Division(l, r)

final case class Product(l: Calculator, r: Calculator) extends Operator(l, r):
  override def method: (Int, Int) => Int = _ * _

  override def copy(l: Calculator, r: Calculator): Operator = Product(l, r)

object Calculator:
  def calculate(source: String): CalculationResult[Int] =
    constructCalculator(source.toList).flatMap(_.value)

  @tailrec
  private def collectLayer(source: List[Char], acc: List[Char] = List(), currentDepth: Int = 0): (List[Char], CalculationResult[Calculator]) =
    if (source.isEmpty)
      (Nil, IncorrectParenthesesSequence.left)
    else
      source.head match
        case '(' => collectLayer(source.tail, source.head :: acc, currentDepth + 1)
        case ')' =>
          if (currentDepth == 0)
            (source.tail, constructCalculator(acc.reverse))
          else
            collectLayer(source.tail, source.head :: acc, currentDepth - 1)
        case _ => collectLayer(source.tail, source.head :: acc, currentDepth)

  @tailrec
  private def constructCalculator(source: List[Char], acc: CalculationResult[Calculator] = Right(EmptyValue)): CalculationResult[Calculator] =
    if (source.isEmpty || acc.isLeft)
      acc.map(_.push)
    else if (source.head.isDigit)
      constructCalculator(source.tail, acc.flatMap(_.append(Number(source.head.getNumericValue).right)))
    else if (source.head == '(')
      val (tail, layer) = collectLayer(source.tail)
      constructCalculator(tail, acc.flatMap(_.append(layer)))
    else
      constructCalculator(source.tail, acc.flatMap(res => getOperator(source.head, res.push)))

  private def getOperator(source: Char, l: Calculator): CalculationResult[Operator] =
    source match
      case '+' => Addition(l, EmptyValue).right
      case '-' => Subtraction(l, EmptyValue).right
      case '/' => Division(l, EmptyValue).right
      case '*' => Product(l, EmptyValue).right
      case _ => UnknownCharacter.left
