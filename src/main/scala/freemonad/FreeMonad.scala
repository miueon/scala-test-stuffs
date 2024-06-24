package freemonad

import cats.Functor
import cats.syntax.all.*
import cats.Monad
import cats.data.Kleisli
import cats.Id

enum Free[F[_], A]:
  case Pure(a: A)
  case Impure[F[_], A](value: F[Free[F, A]]) extends Free[F, A]

object Free:
  def eta[F[_]: Functor, A](fa: F[A]): Free[F, A] =
    Impure(fa.map(Pure(_)))

  given [F[_]: Functor]: Monad[[x] =>> Free[F, x]] with
    def pure[A](x: A): Free[F, A] = Pure(x)
    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa match
        case Pure(a)       => f(a)
        case Impure(value) => Impure(value.map(_.flatMap(f)))
    def tailRecM[A, B](a: A)(f: A => Free[F, Either[A, B]]): Free[F, B] =
      f(a).flatMap {
        case Left(value)  => tailRecM(value)(f)
        case Right(value) => Pure(value)
      }

type State[S, A] = Kleisli[Id, S, A]

type FState[S, A] = Free[[x] =>> State[S, x], A]

object FState:
  def getF[S]: FState[S, S] = Free.eta(Kleisli.ask[Id, S])

  def writeF[S, A](a: A): FState[S, A] = Free.eta(Kleisli.pure(a))
