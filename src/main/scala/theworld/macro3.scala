package theworld.macro3

import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type

def forEachImpl[T](t: Expr[Array[T]], fn: Expr[T => Unit])(using
    Quotes,
    Type[T]
): Expr[Unit] =
  // The following code would expanded literally
  '{
    val xs = $t
    var i = 0
    val n = xs.length
    while i < n do
      ${ Expr.betaReduce('{ $fn(xs(i)) }) } 
      // beta reduce the expr then splice the expr. 
      // Aim for runtime performance
      // As the fn would be reduced to expression, rather than a function call by the betaReduce
      // fn(xs(i))
      i += 1
  }

extension [T](t: Array[T])
  inline def forEach(inline fn: T => Unit): Unit =
    ${ forEachImpl('t, 'fn) }
