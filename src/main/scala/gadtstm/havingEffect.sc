
enum Expr:
  case EInt(n: Int)
  case EInc
  case EApp(e1: Expr, e2: Expr)

object Expr:
  val tinc = EApp(EInc, EApp(EInc, EInt(2)))

  
