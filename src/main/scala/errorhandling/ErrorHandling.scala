package handle
import cats.MonadThrow
import cats.effect.IO
import cats.syntax.all.*
import scala.util.*

type F[A] = IO[A]

val F = MonadThrow[F]

def f[T](a: T): F[T] = F.pure(a)

@main
def test =
  val a = F.raiseError(new RuntimeException).orElse(f(2))
  for
    av <- a
    _ <- IO.print(av)
  yield ()
