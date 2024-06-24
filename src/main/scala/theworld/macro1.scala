package theworld.macro1

import scala.quoted.Quotes
import scala.quoted.Expr

// The essence of macro is
// A inline func and its impl
// Why inline? cause after the macro is expanded, it may not be a func call
// So it must be inline
// But its body must be ${...impl} to call a impl

// quote: like 'expr turn T to Expr[T]
// splice: like $expr turn Expr[T] to T
object Macros:
  inline def test = ${ testImpl }
  def testImpl(using Quotes): Expr[Unit] =
    '{ () }

// def balabala = 
//   Macros.test // should in another file





