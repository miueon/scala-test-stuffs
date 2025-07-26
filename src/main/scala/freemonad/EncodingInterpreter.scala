package freemonad

import cats.Functor
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import effect.free.~>

enum ExprF[A]:
  case Lit(n: Int, next: Int => A)
  case Add(x: Int, y: Int, next: Int => A)
  case Mul(x: Int, y: Int, next: Int => A)

given Functor[ExprF] with
  def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] =
    fa match
      case ExprF.Lit(n, next)  => ExprF.Lit(n, next andThen f)
      case ExprF.Add(x, y, nx) => ExprF.Add(x, y, nx andThen f)
      case ExprF.Mul(x, y, nx) => ExprF.Mul(x, y, nx andThen f)

def lit(n: Int): effect.free.Free[ExprF, Int] = effect.free.Free.Suspend(ExprF.Lit(n, identity))
def add(x: Int, y: Int): effect.free.Free[ExprF, Int] =
  effect.free.Free.Suspend(ExprF.Add(x, y, identity))

def mul(x: Int, y: Int): effect.free.Free[ExprF, Int] =
  effect.free.Free.Suspend(ExprF.Mul(x, y, identity))

val prog: effect.free.Free[ExprF, Int] =
  for
    a <- lit(1)
    b <- add(2, 3)
    c <- mul(a, b)
  yield c

val toIO: ExprF ~> IO = [A] => (fa: ExprF[A]) => fa match
  case ExprF.Lit(n, next) => IO.pure(next(n))
  case ExprF.Add(x, y, next) => IO.pure(next(x + y))
  case ExprF.Mul(x, y, next) => IO.pure(next(x * y))

object EncodingInterpreterApp extends IOApp:
  def run(args: List[String]): IO[ExitCode] =
    for
      result <- prog.foldMap[IO, Int](toIO)
      _ <- IO.println(s"Result: $result")
    yield ExitCode.Success 

