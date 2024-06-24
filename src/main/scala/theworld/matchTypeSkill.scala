package theworld.matchtypeskill
import scala.compiletime.ops.int.<=
import scala.compiletime.*
import scala.annotation.implicitNotFound

@implicitNotFound("Unexpected case ${T}.")
class Expected[T <: Boolean] private ()
object Expected:
  given Expected[true] = new Expected[true]
// we need to compare the Type
// S <= T: <= from compiletime ops
// now we need someone to check the result
// implicit with predifined instance to restrict the case
// finally cast literal value to value by constValue
inline def range[S <: Int, T <: Int](using Expected[S <= T]) =
  constValue[S].to(constValue[T])

// val a = range[4, 2] // Unexpected case (4 : Int) <= (2 : Int)
// Tuple can also be a type
// like constValue to cast Type to value
// erasedValue can cast literal type to value in complile time
// Since the Xs is a tuple, so we cannot assume the type of its element
inline def joinStr[D <: String, Xs <: Tuple]: String =
  inline erasedValue[Xs] match
    case _: EmptyTuple        => ""
    case _: (t *: EmptyTuple) => constValue[t & String]
    case _: (t *: ts) => constValue[t & String] + constValue[D] + joinStr[D, ts]

val s = joinStr["+", ("1", "2", "3")] // 1+2+3

infix type ~>[F[_], G[_]] = [t] => F[t] => G[t]
type Id[T] = T
val wrap: Id ~> Option = [t] => (x: t) => Some(x)
val unwrap: Option ~> Id = [t] => (x: Option[t]) => x.get
val lstOpt: List ~> Option = [t] => (x: List[t]) => x.headOption
val unOptLst: Option ~> List = [t] => (x: Option[t]) => x.toList

@main
def test =
  println(s) // 1+2+3
  val a = 10
  val aOpt = wrap(a)
  val aLstOpt = lstOpt(List(1))
