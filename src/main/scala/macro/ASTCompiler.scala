
package backend

import backend.*

object ASTCompiler:
  def compile(awkRootExpr: AwkQueryAST): String = awkRootExpr match
    case AwkQueryAST(file) =>
      "awk '{print $0}' " + file
    case AwkQueryAST(file, AwkMapExpr.MapExpr(expr)) =>
      val toPrint = expr.idx.map(x => "$" + x).mkString(",")
      s"""awk '{print $toPrint }' $file"""

    
  