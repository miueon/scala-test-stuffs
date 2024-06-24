package tplamb
import cats.Functor
import cats.Applicative
import cats.syntax.all.*

case class MFC[A, B](a: A, b: B)

given Functor[[x] =>> MFC[?, x]] with
  def map[A, B](fa: MFC[?, A])(f: A => B): MFC[?, B] = 
    MFC(fa.a, f(fa.b))


