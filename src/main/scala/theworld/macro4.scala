package theworld.macro4

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type

case class Ref[@specialized(Specializable.BestOfBreed) T](
    get: () => T,
    set: T => Unit
):
  def :=(v: T) = set(v)
  def value: T = get()
  override def toString(): String = value.toString()

object Ref:
  given [T]: Conversion[Ref[T], T] = _.value

  import scala.quoted.*
  inline def apply[T](inline v: T): Ref[T] =
    ${ make('v) }

  def make[T](v: Expr[T])(using Quotes, Type[T]): Expr[Ref[T]] =
    import quotes.reflect.{Ref as ReflectRef, *}
    v.asTerm match
      // here we declare the v is inlined, so we can check the term of v
      // then using Assign to use the original v as lhs nv as rhs
      case Inlined(_, _, term @ (Ident(_) | Select(_, _))) =>
        '{
          Ref(() => $v, nv => ${ Assign(term, 'nv.asTerm).asExpr })
        }
      case _ =>
        report.throwError(s"${v.show} is not an assignable value.", v)


