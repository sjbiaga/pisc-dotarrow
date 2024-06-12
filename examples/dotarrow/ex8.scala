import cats.effect.{ IO, IOApp }

case class C(m: Int, n: Int)

object App extends IOApp.Simple:

  override def run: IO[Unit] =

    for
      j: Int <- IO { 4 - 3  }
      k: Int <- IO { j + 1 + 3 }
      x: Int <- IO { 2 + k * 1 - 0 }
      y: Int <- IO { x * 4 * (x - x + 1) }
      z <- IO { C(x * y, 1 * 1) }
      C(a, b) = z
      w: Int <- IO { (a * b - 0) * 1  }
      _u: Unit <- IO { println(w) }
      _ <- IO { }.void
    yield
      ()
