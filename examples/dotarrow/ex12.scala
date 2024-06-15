package dotarrow
package codec.idem
package ex1

import cats.effect.{ IO, IOApp }
import App.NumericEncoding.{ mkNumericOps, fromInt }

case class C(m: Int, n: Int, p: Int)
case class D(q: Int, r: Int)

object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x <- IO { -(1 + 4 - 1) :: 2 + 5 - 1 :: 3 + 6 - 1 :: Nil }
      C(a, b, c) = to[C](x)
      y <- IO { a + c :: b + 7 :: Nil }
      D(d, e) = to[D](y)
      z: Int <- IO { a + b + d - c * e }
      _u: Unit <- IO { println(z) }
      _ <- IO { }.void
    yield
      ()

  def to[T](it: List[Int]): T = ???
  def to[T](it: List[Encoding])(using Conversion[List[Encoding], T]): T = it

  given Conversion[List[Encoding], C] = {
    case List(m, n, p) =>
      C(m.expr(), n.expr(), p.expr())
  }
  given Conversion[List[Encoding], D] = {
    case List(q, r) =>
      D(q.expr(), r.expr())
  }

  import Expr.Op
  import Op._

  object Expr:

    enum Op:
       case Add, Sub, Mul
       override def toString = this match
         case Add => "+"
         case Sub => "-"
         case Mul => "*"

  sealed trait Expr:

    final def apply(): Int =
      this match
        case Val(n) => n
        case Bin(lhs, Add, rhs) => lhs() + rhs()
        case Bin(lhs, Sub, rhs) => lhs() - rhs()
        case Bin(lhs, Mul, rhs) => lhs() * rhs()
        case Neg(exp) => -exp()

  case class Val(number: Int) extends Expr:
    override def toString = number.toString

  case class Bin(lhs: Expr, op: Op, rhs: Expr) extends Expr:
    override def toString =
      val l = lhs match
        case _: Val => s"$lhs"
        case _ => s"($lhs)"
      val r = rhs match
        case _: Val => s"$rhs"
        case _ => s"($rhs)"
      l + s" $op " +  r

  case class Neg(exp: Expr) extends Expr:
    override def toString =
      exp match
        case Val(0) => "0"
        case Val(n) if n > 0 => s"-$n"
        case _ => s"-($exp)"


  case class Encoding(expr: Expr):
    override def toString = expr.toString

  implicit object NumericEncoding extends Numeric[Encoding]:
    def fromInt(n: Int) = Encoding(Val(n))
    def minus(lhs: Encoding, rhs: Encoding) = Encoding(Bin(lhs.expr, Sub, rhs.expr))
    def negate(enc: Encoding) = Encoding(Neg(enc.expr))
    def plus(lhs: Encoding, rhs: Encoding) = Encoding(Bin(lhs.expr, Add, rhs.expr))
    def times(lhs: Encoding, rhs: Encoding) = Encoding(Bin(lhs.expr, Mul, rhs.expr))
    def parseString(str: String) = ???
    def toDouble(enc: Encoding) = ???
    def toFloat(enc: Encoding) = ???
    def toInt(enc: Encoding) = ???
    def toLong(enc: Encoding) = ???
    def compare(lhs: Encoding, rhs: Encoding): Int = lhs.expr() - rhs.expr()
