import cats.effect.{ IO, IOApp }

case class C(m: Int, n: Int)
case class D(p: Int, q: Int)

object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      x: Int <- IO { 2 + (4 - 3 + 1 + 3) * 1 - 0 }
      y: Int <- IO { x * 4 * (x - x + 1) }
      z <- IO { C(x * y, 1 * 1) }
      C(a, b) = z
      w <- IO { D(a * b - 0, 1) }
      D(c, d) = w
      _u: Unit <- IO { println(c * d) }
      _ <- IO { }.void
    yield
      ()
