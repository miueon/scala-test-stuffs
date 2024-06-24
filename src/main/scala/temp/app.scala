package temp.app
import cats.Applicative
import cats.Functor
import cats.Monad
import cats.kernel.Monoid
import cats.syntax.all.*
import scala.annotation.tailrec
import cats.instances.tailRec

case class Acc[O, A](acc: O)

object Acc:
  given [O]: Functor[[A] =>> Acc[O, A]] with
    def map[A, B](fa: Acc[O, A])(f: A => B): Acc[O, B] =
      fa.asInstanceOf[Acc[O, B]]

  given [O: Monoid]: Applicative[[A] =>> Acc[O, A]] with
    def pure[A](x: A): Acc[O, A] = Acc(Monoid[O].empty)

    def ap[A, B](ff: Acc[O, A => B])(fa: Acc[O, A]): Acc[O, B] =
      Acc(Monoid[O].combine(ff.acc, fa.acc))

enum Maybe[+A]:
  case Noth extends Maybe[Nothing]
  case Just[A](a: A) extends Maybe[A]
import Maybe.*
object Maybe:
  given Functor[Maybe] with
    def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] = fa match
      case Just(a) => Just(f(a))
      case Noth    => Noth

  given Applicative[Maybe] with
    def ap[A, B](ff: Maybe[A => B])(fa: Maybe[A]): Maybe[B] = ff match
      case Noth    => Noth
      case Just(f) => fa.map(f)
    def pure[A](x: A): Maybe[A] = Just(x)

  given Monad[Maybe] with
    def pure[A](x: A): Maybe[A] = Just(x)
    def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = fa match
      case Noth    => Noth
      case Just(a) => f(a)

    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Maybe[Either[A, B]]): Maybe[B] =
      f(a) match
        case Just(Left(a1)) => tailRecM(a1)(f)
        case Just(Right(b)) => Just(b)
        case Noth           => Noth
def iffy[F[_]: Applicative, A](fb: F[Boolean])(ft: F[A])(fe: F[A]): F[A] =

  (fb, ft, fe).mapN { (b, t, e) =>
    if b then t else e
  }

def miffy[F[_]: Monad, A](mb: F[Boolean])(mt: F[A])(me: F[A]): F[A] =
  for
    b <- mb
    r <- if b then mt else me
  yield r

@main
def test = 
  val a = iffy(Just(true))(Just(2))(Noth)
  println(a)

  val b = miffy(Just(true))(Just(2))(Noth)
  println(b)

