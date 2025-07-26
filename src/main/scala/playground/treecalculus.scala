package treecalculus

// doc https://treecalcul.us/specification/

enum T:
  case Leaf
  case Stem(t: T)
  case Fork(t1: T, t2: T)

export T.*

object T:
  def reduce(a: T, b: T): T = 
    a match
      case Leaf => Stem(b)
      case Stem(t) => Fork(a, b)
      case Fork(Leaf, a) => a
      case Fork(Stem(a1), a2) => reduce(reduce(a1, b), reduce(a2, b))
      case Fork(Fork(a1, a2), a3) => 
        b match
          case Leaf => a1
          case Stem(u) => reduce(a2, u)
          case Fork(u, v) => reduce(reduce(a3, u), v)

val _false = Leaf
val _true = Stem(Leaf)
val _not = Fork(Fork(_true, Fork(Leaf, _false)), Leaf)

@main
def test =
  println(reduce(_not, _false) == _true)
  println(reduce(_not, _true) )
