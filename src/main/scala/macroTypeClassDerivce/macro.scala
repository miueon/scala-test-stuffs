package mtc

import scala.quoted.*
inline def deriveShow[T]: Show[T] = ${ deriveShowImpl[T] }
def deriveShowImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[Show[T]] =
  import quotes.reflect.*
  val tpeSym = TypeTree.of[T].symbol
  if tpeSym.flags.is(Flags.Case) then deriveCaseClassShow[T]
  else if tpeSym.flags.is(Flags.Trait & Flags.Sealed) then deriveTraitShow[T]
  else
    throw RuntimeException(
      s"Unsupported combination of flags: ${tpeSym.flags.show}"
    )

def deriveTraitShow[T](using quotes: Quotes, tpe: Type[T]): Expr[Show[T]] =
  import quotes.reflect.*
  val children = TypeTree.of[T].symbol.children
  // TODO check what can we get from the children
  def showBody(t: Expr[T]): Expr[String] =
    val selector = t.asTerm // t here should be a sealed trait
    val ifBranches = children.map { sym =>
      // TODO check what symbol is
      val childTpe = TypeIdent(sym)
      val condition =
        TypeApply(Select.unique(selector, "isInstanceOf"), childTpe :: Nil)
      // TODO what the typeapply do?
      val action = applyShow(
        lookupShowFor(childTpe.tpe),
        Select.unique(selector, "asInstanceOf").appliedToType(childTpe.tpe)
      )
      condition -> action
    }
    mkIfStatement(ifBranches).asExprOf[String]
  '{
    new Show[T]:
      def show(t: T): String = ${ showBody('{ t }) }
  }

def deriveCaseClassShow[T](using quotes: Quotes, tpe: Type[T]): Expr[Show[T]] =
  import quotes.reflect.*
  val fields = TypeTree.of[T].symbol.caseFields
  def showField(caseClassTerm: Term, field: Symbol): Expr[String] =
    val fieldValDef: ValDef = field.tree.asInstanceOf[ValDef]
    val fieldTpe = fieldValDef.tpt.tpe
    val fieldName = fieldValDef.name

    val tcl = lookupShowFor(fieldTpe) // Show[$fieldTpe]
    val fieldValue = Select(caseClassTerm, field) // v.field
    val strRepr = applyShow(tcl, fieldValue).asExprOf[String]
    '{
      s"${${ Expr(fieldName) }}: ${${ strRepr }}"
    } // summon[Show[$fieldTpe]].show(v.field)

  def showBody(v: Expr[T]): Expr[String] =
    val vTerm = v.asTerm
    val valueExprs = fields.map(showField(vTerm, _))
    val exprOfList = Expr.ofList(valueExprs)
    '{ $exprOfList.mkString(", ") }

  '{
    new Show[T]:
      def show(t: T): String =
        s"{ ${${ showBody('{ t }) }}}"
  }
def lookupShowFor(using quotes: Quotes)(
    t: quotes.reflect.TypeRepr
): quotes.reflect.Term =
  import quotes.reflect.*
  val showTpe = TypeRepr.of[Show]
  val tclTpe = showTpe.appliedTo(t)
  Implicits.search(tclTpe) match
    case res: ImplicitSearchSuccess =>
      res.tree

def applyShow(using
    quotes: Quotes
)(tcl: quotes.reflect.Term, arg: quotes.reflect.Term): quotes.reflect.Term =
  import quotes.reflect.*
  Apply(Select.unique(tcl, "show"), arg :: Nil)

def mkIfStatement(using quotes: Quotes)(
    branches: List[(quotes.reflect.Term, quotes.reflect.Term)]
): quotes.reflect.Term =
  import quotes.reflect.*
  branches match
    case (p1, a1) :: xs =>
      If(p1, a1, mkIfStatement(xs))
    case Nil =>
      ('{
        throw RuntimeException(
          "Unhandled condition encountered during Show drivation"
        )
      }).asTerm
