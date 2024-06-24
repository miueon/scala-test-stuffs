package theworld.macro2

import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Varargs

def joinImpl(delim: Expr[String], xs: Expr[Seq[String]])(using Quotes) = 
  val d = delim.valueOrAbort
  val strs = xs match
    case Varargs(ys) => ys.map(_.valueOrAbort)
    case _ => xs.valueOrAbort

  val result = strs.mkString(d)
  Expr(result)

inline def joinStr(inline delim: String)(inline xs: String*): String = 
  ${ joinImpl('delim, 'xs) }

// val sep = joinStr(",")("1", "2", "3") 
// val sep = "1,2,3"

