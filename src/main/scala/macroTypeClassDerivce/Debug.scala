package mtc

import scala.quoted.*

object Debug:
  def printImpl[A: Type](a: Expr[A])(using q: Quotes): Expr[A] =
    import q.*, q.reflect.*
    report.info(a.asTerm.show(using Printer.TreeAnsiCode))
    a

  inline def print[A](inline a: A): A = ${ printImpl('{ a }) }
