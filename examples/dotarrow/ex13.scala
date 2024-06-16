package dotarrow
package codec.idem
package ex13

import scala.util.control.TailCalls.{ done, tailcall, TailRec }

import cats.effect.{ IO, IOApp }
import App.NumericEncoding.{ mkNumericOps, fromInt }


case class C(m: Int, n: Int, p: Int)
case class D(q: Int, r: Int)

object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x <- IO { -1 * 1 * 2 + -2 * 5 + -3 * 4 - -3 * 2 + -4 * 2 * 3 * 1 :: 0 - 1 + 9 :: -3 * 3 :: Nil }
      C(a, b, c) = to[C](x)
      y <- IO { -1 * (a - 55) * c + -2 * b - -4 * (a - 50) * b * 1 :: -4 * b * c * (1 * 1) :: Nil }
      D(d, e) = to[D](y)
      z: Int <- IO { a + b + d - c * e }
      _u: Unit <- IO { println(z) }
      _ <- IO { }.void
    yield
      ()

  object Fun:

    // -1 Ackermann
    def ack(m: Int, n: Int): Int = ack_(m, n).result
    private def ack_(m: Int, n: Int): TailRec[Int] =
      if m == 0 then done(n+1)
      else if n == 0 then tailcall { ack_(m-1, 1) }
      else
        for
          p <- tailcall { ack_(m, n-1) }
          q <- tailcall { ack_(m-1, p) }
        yield
          q

    // -2 Fibonacci
    def fib(n: Int): Int = fib_(n).result
    private def fib_(n: Int): TailRec[Int] =
      if n < 2 then done(n)
      else
        for
          p <- tailcall { fib_(n-1) }
          q <- tailcall { fib_(n-2) }
        yield
          p+q

    // -3 factorial
    def fac(n: Int): Int = fac_(n).result
    private def fac_(n: Int): TailRec[Int] =
      if n == 0 then done(1)
      else
        for
          p <- tailcall { fac_(n-1) }
        yield
          n*p

    // -4 Sudan
    def sud(m: Int, n: Int, p: Int): Int = sud_(m, n, p).result
    private def sud_(m: Int, n: Int, p: Int): TailRec[Int] =
      if p == 0 then done(m+n)
      else if n == 0 then done(m)
      else
        for
          q <- tailcall { sud_(m, n-1, p) }
          r <- tailcall { sud_(q, n, 0) }
          s <- tailcall { sud_(q, r, p-1) }
        yield
          s

  def to[T](it: List[Int]): T = ???
  def to[T](it: List[Encoding])(using Conversion[List[Encoding], T]): T = it

  given Conversion[List[Encoding], C] = {
    case List(Encoding(Left(m)), Encoding(Left(n)), Encoding(Left(p))) =>
      C(m(), n(), p())
  }
  given Conversion[List[Encoding], D] = {
    case List(Encoding(Left(q)), Encoding(Left(r))) =>
      D(q(), r())
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
        case Fun(List(Val(-1), m, n)) => (m(), n()) match
          case (m, n) if m >= 0 && n >= 0 => Fun.ack(m, n)
        case Fun(List(Val(-2), n)) => n() match
          case n if n >= 0 => Fun.fib(n)
        case Fun(List(Val(-3), n)) => n() match
          case n if n >= 0 => Fun.fac(n)
        case Fun(List(Val(-4), m, n, p)) => (m(), n(), p()) match
          case (m, n, p) if m >= 0 && n >= 0 && p >= 0 => Fun.sud(m, n, p)

  case class Val(number: Int) extends Expr:
    override def toString = number.toString

  case class Fun(args: List[Expr]) extends Expr:
    override def toString =
      val n = args.head match
        case Val(-1) => "ack"
        case Val(-2) => "fib"
        case Val(-3) => "fac"
        case Val(-4) => "sud"
      s"$n(${args.tail.mkString(", ")})"

  case class Bin(lhs: Expr, op: Op, rhs: Expr) extends Expr:
    override def toString =
      val l = lhs match
        case _: Val | _: Fun => s"$lhs"
        case _ => s"($lhs)"
      val r = rhs match
        case _: Val | _: Fun => s"$rhs"
        case _ => s"($rhs)"
      l + s" $op " +  r

  case class Neg(exp: Expr) extends Expr:
    override def toString =
      exp match
        case Val(0) => "0"
        case Val(n) if n > 0 => s"-$n"
        case _ => s"-($exp)"


  case class Encoding(expr: Expr Either List[Expr]):
    override def toString = expr match
      case Left(it) => it.toString
      case Right(it) => it.toString

  implicit object NumericEncoding extends Numeric[Encoding]:
    private val fun: List[Expr] => Expr Either List[Expr] = {
      case List(Val(-2 | -3), _, _, _*) => ???
      case List(Val(-1), _, _, _, _*) => ???
      case List(Val(-4), _, _, _, _, _*) => ???
      case it @ List(Val(-2 | -3), _) => Left(Fun(it))
      case it @ List(Val(-1), _, _) => Left(Fun(it))
      case it @ List(Val(-4), _, _, _) => Left(Fun(it))
      case it @ List(Val(n), _*) if n < 0 => Right(it)
      case it => Left(it.reduce(Bin(_, Mul, _)))
    }
    private def bin(lhs: Encoding, op: Op, rhs: Encoding): Encoding =
      (lhs.expr, rhs.expr) match
        case (Left(l), Left(r)) =>
          Encoding(Left(Bin(l, op, r)))
        case (Right(List(l @ Val(n))), Right(List(r: Val))) if n >= 0 =>
          Encoding(Left(Bin(l, op, r)))
        case (Right(List(l @ Val(n))), Left(r)) if n >= 0 =>
          Encoding(Left(Bin(l, op, r)))
        case (Left(l), Right(List(r: Val))) =>
          Encoding(Left(Bin(l, op, r)))
        case _ => ???
    def fromInt(n: Int) = Encoding(Right(List(Val(n))))
    def minus(lhs: Encoding, rhs: Encoding) = bin(lhs, Sub, rhs)
    def negate(rhs: Encoding) =
      rhs.expr match
        case Left(l) => Encoding(Left(Neg(l)))
        case Right(List(r @ Val(n))) if n >= 0 => Encoding(Left(Neg(r)))
        case _ => ???
    def plus(lhs: Encoding, rhs: Encoding) = bin(lhs, Add, rhs)
    def times(lhs: Encoding, rhs: Encoding) =
      (lhs.expr, rhs.expr) match
        case (Right(l), Right(r)) => Encoding(fun(l ++ r))
        case (Right(l), Left(r)) =>
          fun(l) match
            case Right(it) => Encoding(fun(it :+ r))
            case Left(it) => Encoding(Left(Bin(it, Mul, r)))
        case (Left(l), Right(r)) =>
          fun(r) match
            case Right(_) => ???
            case Left(it) => Encoding(Left(Bin(l, Mul, it)))
        case _ => bin(lhs, Mul, rhs)
    def parseString(str: String) = ???
    def toDouble(enc: Encoding) = ???
    def toFloat(enc: Encoding) = ???
    def toInt(enc: Encoding) = ???
    def toLong(enc: Encoding) = ???
    def compare(lhs: Encoding, rhs: Encoding): Int =
      (lhs.expr, rhs.expr) match
        case (Left(l), Left(r)) => l() - r()
