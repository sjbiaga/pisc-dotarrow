package dotarrow
package codec.list
package ex1

import cats.effect.{ IO, IOApp }
import App.NumericEncoding.{ mkNumericOps, fromInt }

case class C(m: Int, n: Int, p: Int)
case class D(q: Int, r: Int)

object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x <- IO { -(1 * 2 * 3) + 4 * 5 * 6 - 1 * 1 * 1 }
      C(a, b, c) = to[C](x)
      y <- IO { a * b + c * 7 }
      D(d, e) = to[D](y)
      z: Int <- IO { a + b + d - c * e }
      _u: Unit <- IO { println(z) }
      _ <- IO { }.void
    yield
      ()

  def to[T](it: Int): T = ???
  def to[T](it: Encoding)(using Conversion[Encoding, T]): T = it

  given Conversion[Encoding, C] = { case Encoding(List(m, n, p)) => C(m, n, p) }
  given Conversion[Encoding, D] = { case Encoding(List(q, r)) => D(q, r) }

  case class Encoding(ns: List[Int]):
    override def toString = ns.mkString(" * ")

  implicit object NumericEncoding extends Numeric[Encoding]:
    def fromInt(n: Int) = Encoding(List(n))
    def minus(lhs: Encoding, rhs: Encoding) = Encoding((lhs.ns zip rhs.ns).map(_ - _))
    def negate(enc: Encoding) = Encoding(enc.ns.reverse)
    def plus(lhs: Encoding, rhs: Encoding) = Encoding((lhs.ns zip rhs.ns).map(_ + _))
    def times(lhs: Encoding, rhs: Encoding) = Encoding(lhs.ns ++ rhs.ns)
    def parseString(str: String) = ???
    def toDouble(enc: Encoding) = ???
    def toFloat(enc: Encoding) = ???
    def toInt(enc: Encoding) = ???
    def toLong(enc: Encoding) = ???
    def compare(lhs: Encoding, rhs: Encoding): Int =
      require(lhs.ns.size == rhs.ns.size)
      (lhs - rhs).ns.dropWhile(_ == 0).headOption.getOrElse(0)
