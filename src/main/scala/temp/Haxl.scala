package temp.haxl

import cats.Functor
import cats.effect.IO
import cats.effect.kernel.Ref
import cats.syntax.all.*
import cats.Applicative
import cats.kernel.Monoid

trait Request[A]
sealed trait FetchedStatus[A]

final case class BlockedRequest[A](
    request: Request[A],
    ref: Ref[IO, FetchedStatus[A]]
)
object BlockedRequest:
  given [A]: Monoid[BlockedRequest[A]] with
    def empty: BlockedRequest[A] = ???
    def combine(x: BlockedRequest[A], y: BlockedRequest[A]): BlockedRequest[A] = ???

final case class Haxl[A](unHaxl: IO[Result[A]])
enum Result[A]:
  case Done(value: A) extends Result[A]
  case Blocked(req: BlockedRequest[A], c: Haxl[A]) extends Result[A]

object Haxl:
  import Result.*
  given Functor[Haxl] with
    def map[A, B](fa: Haxl[A])(f: A => B): Haxl[B] =
      Haxl(fa.unHaxl.flatMap {
        case Done(a) => IO.pure(Done(f(a)))
        case Blocked(req, c) =>
          IO.pure(Blocked(req.asInstanceOf[BlockedRequest[B]], c.map(f)))
      })

  given Applicative[Haxl] with
    def ap[A, B](ff: Haxl[A => B])(fa: Haxl[A]): Haxl[B] =
      // Haxl {
      //   (ff.unHaxl, fa.unHaxl).parMapN {
      //     case (Done(f), Done(a)) => Done(f(a))
      //     case (Done(f), Blocked(req, c)) =>
      //       Blocked(req.asInstanceOf[BlockedRequest[B]], c.map(f))
      //     case (Blocked(req, c), Done(a)) =>
      //       Blocked(req.asInstanceOf[BlockedRequest[B]], c.map(_(a)))
      //     case (Blocked(req1, c1), Blocked(req2, c2)) =>
      //       Blocked(req1 |+| req2 , c1.ap(c2))
      //   }
      // }
      ???
    def pure[A](x: A): Haxl[A] = Haxl(IO.pure(Done(x)))
