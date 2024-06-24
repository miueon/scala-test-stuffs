package backend

case class AwkQueryAST(
    fileNameOrPath: String,
    mapFilterExpr: AwkMapExpr*
)

case class Projection(idx: Int*)
enum AwkMapExpr:
  case MapExpr(l: Projection)
  case CompositeMap(exprs: List[MapExpr])


