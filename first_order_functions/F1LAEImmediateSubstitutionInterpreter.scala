package first_order_functions

object F1LAEImmediateSubstitutionInterpreter:

  sealed abstract class F1LAE
  case class Num(n: Int) extends F1LAE
  case class Add(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Sub(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Let(name: String, namedExpr: F1LAE, body: F1LAE) extends F1LAE
  case class Id(name: String) extends F1LAE
  case class App(funName: String, arg: F1LAE) extends F1LAE

  case class FunDef(argName: String, body: F1LAE)

  def subst(expr: F1LAE, substId: String, value: F1LAE): F1LAE = expr match
    case Num(n)        => expr
    case Add(lhs, rhs) => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs) => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Let(boundId, namedExpr, boundBody) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then Let(boundId, substNamedExpr, boundBody)
      else Let(boundId, substNamedExpr, subst(boundBody, substId, value))
    case Id(name) =>
      if substId == name then value
      else expr
    case App(funName, argExpr) => App(funName, subst(argExpr, substId, value))

  def interp(expr: F1LAE, funDefs: Map[String, FunDef]): Int = expr match
    case Num(n)        => n
    case Add(lhs, rhs) => interp(lhs, funDefs) + interp(rhs, funDefs)
    case Sub(lhs, rhs) => interp(lhs, funDefs) - interp(rhs, funDefs)
    case Let(boundId, namedExpr, boundBody) =>
      val body = subst(boundBody, boundId, Num(interp(namedExpr, funDefs)))
      interp(body, funDefs)
    case Id(name) => sys.error("found unbound id " + name)
    case App(funName, argExpr) =>
      funDefs(funName) match
        case FunDef(argName, body) =>
          interp(subst(body, argName, Num(interp(argExpr, funDefs))), funDefs)

  given Conversion[String, F1LAE] with
    def apply(s: String): F1LAE = Id(s)
  given Conversion[Int, F1LAE] with
    def apply(n: Int): F1LAE = Num(n)
