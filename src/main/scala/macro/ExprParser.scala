import scala.quoted.*
import scala.quoted.Quotes

object ExprParser:
  def from[T: Type, R: Type](f: Expr[T => R])(using
      Quotes
  ): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }

  def conmputeIndex[T](expr: Expr[?])(using Quotes, Type[T]) =
    val quoted = implicitly[Quotes]
    import quoted.reflect.*

    expr.asTerm match
      case Select(Ident(_), propertyName) =>
        val tpe = TypeRepr.of[T]
        if tpe.classSymbol.get.name == "Tuple2" then
          if propertyName == "_1" then 1
          else 2
        else
          tpe.classSymbol.get.caseFields.zipWithIndex
            .find(_._1.name == propertyName)
            .map(_._2)
            .get + 1
      case Inlined(_, _, Block(_, Select(Ident(_), propertyName))) =>
        val tpe = TypeRepr.of[T]
        tpe.classSymbol.get.caseFields.zipWithIndex
          .find(_._1.name == propertyName)
          .map(_._2)
          .get + 1
