package backend

import scala.annotation.tailrec

object ASTNormalizer:
  private def expandComposite(
      exprs: List[AwkMapExpr]
  ): List[AwkMapExpr] =
    exprs match
      case h :: tail =>
        h match
          case AwkMapExpr.CompositeMap(innerExpr) =>
            innerExpr ::: exprs
          case _ =>
            h :: expandComposite(tail)
      case Nil => Nil

  private def mergeProjections(exprs: List[AwkMapExpr]): List[AwkMapExpr] =
    exprs match
      case AwkMapExpr.MapExpr(l1) :: AwkMapExpr.MapExpr(l2) :: tail =>
        val m1 = Map(l1.idx.zipWithIndex.map(x => ((x._2 + 1) -> x._1))*)
        AwkMapExpr.MapExpr(Projection(l2.idx.map(prj => m1(prj))*)) :: tail
      case h :: tail => h :: mergeProjections(tail)
      case Nil       => Nil

  @tailrec
  private def normalizeList(
      exprs: List[AwkMapExpr]
  ): List[AwkMapExpr] =
    val result =
      expandComposite.andThen(mergeProjections)(exprs)

    if result != exprs then normalizeList(result)
    else result

  def normalize(awkRootExpr: AwkQueryAST) = AwkQueryAST(
    awkRootExpr.fileNameOrPath,
    normalizeList(awkRootExpr.mapFilterExpr.toList)*
  )
