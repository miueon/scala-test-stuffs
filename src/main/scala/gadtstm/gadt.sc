enum Pair[T]:
  case IPair(x: Expr[Int], y: Expr[Int]) extends Pair[Int]
  case BPair(x: Expr[Boolean], y: Expr[Boolean]) extends Pair[Boolean]
enum Expr[T]:
  case IExpr(n: Int) extends Expr[Int]
  case BExpr(b: Boolean) extends Expr[Boolean]
  case Add(x: Expr[Int], y: Expr[Int]) extends Expr[Int]
  case Eq(p: Pair[T]) extends Expr[Boolean]

def evalInt(e: Expr[Int]): Int = e match
  case Expr.IExpr(n)  => n
  case Expr.Add(x, y) => evalInt(x) + evalInt(y)

def evalBool(e: Expr[Boolean]): Boolean = e match
  case Expr.BExpr(b)             => b
  case Expr.Eq(Pair.BPair(x, y)) => evalBool(x) == evalBool(y)
  case Expr.Eq(Pair.IPair(x, y)) => evalInt(x) == evalInt(y)

println(evalBool(Expr.Eq(Pair.IPair(Expr.IExpr(1), Expr.IExpr(2)))))
