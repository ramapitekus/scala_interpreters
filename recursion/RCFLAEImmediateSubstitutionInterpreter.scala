package recursion

object RCFLAEImmediateSubstitutionInterpreter:

  sealed abstract class RCFLAE
  case class Num(n: Int) extends RCFLAE
  case class Add(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Sub(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Mult(lhs: RCFLAE, rhs: RCFLAE) extends RCFLAE
  case class Let(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class Rec(name: String, namedExpr: RCFLAE, body: RCFLAE) extends RCFLAE
  case class Id(name: String) extends RCFLAE
  case class If0(test: RCFLAE, posBody: RCFLAE, negBody: RCFLAE) extends RCFLAE
  case class Fun(param: String, body: RCFLAE) extends RCFLAE
  case class App(funExpr: RCFLAE, arg: RCFLAE) extends RCFLAE

  def subst(expr: RCFLAE, substId: String, value: RCFLAE): RCFLAE = expr match
    case Num(n)         => expr
    case Add(lhs, rhs)  => Add(subst(lhs, substId, value), subst(rhs, substId, value))
    case Sub(lhs, rhs)  => Sub(subst(lhs, substId, value), subst(rhs, substId, value))
    case Mult(lhs, rhs) => Mult(subst(lhs, substId, value), subst(rhs, substId, value))
    case Let(boundId, namedExpr, boundBody) =>
      val substNamedExpr = subst(namedExpr, substId, value)
      if boundId == substId then Let(boundId, substNamedExpr, boundBody)
      else Let(boundId, substNamedExpr, subst(boundBody, substId, value))
    case Rec(boundId, namedExpr, boundBody) =>
      if boundId == substId then expr
      else Rec(boundId, subst(namedExpr, substId, value), subst(boundBody, substId, value))
    case Id(name) =>
      if substId == name then value
      else expr
    case If0(test, posBody, negBody) =>
      If0(
        subst(test, substId, value),
        subst(posBody, substId, value),
        subst(negBody, substId, value)
      )
    case App(funExpr, argExpr) =>
      App(subst(funExpr, substId, value), subst(argExpr, substId, value))
    case Fun(arg, body) =>
      if arg == substId then expr
      else Fun(arg, subst(body, substId, value))

  def interp(expr: RCFLAE): RCFLAE = expr match
    case Num(n) => expr
    case Add(lhs, rhs) =>
      val lhsv = interp(lhs)
      lhsv match
        case Num(n) =>
          val rhsv = interp(rhs)
          rhsv match
            case Num(m) => Num(n + m)
            case _      => sys.error("Can only add numbers")
        case _ => sys.error("Can only add numbers")
    case Sub(lhs, rhs) =>
      val lhsv = interp(lhs)
      lhsv match
        case Num(n) =>
          val rhsv = interp(rhs)
          rhsv match
            case Num(m) => Num(n - m)
            case _      => sys.error("Can only subtract numbers")
        case _ => sys.error("Can only subtract numbers")
    case Mult(lhs, rhs) =>
      val lhsv = interp(lhs)
      lhsv match
        case Num(n) =>
          val rhsv = interp(rhs)
          rhsv match
            case Num(m) => Num(n * m)
            case _      => sys.error("Can only subtract numbers")
        case _ => sys.error("Can only subtract numbers")
    case Let(boundId, namedExpr, boundBody) =>
      interp(subst(boundBody, boundId, interp(namedExpr)))
    case Rec(boundId, namedExpr, boundBody) =>
      val recExpr = Rec(boundId, namedExpr, Id(boundId))
      val transformedNamedExpr = subst(namedExpr, boundId, recExpr)
      interp(subst(boundBody, boundId, interp(transformedNamedExpr)))
    case Id(name) => sys.error("found unbound id " + name)
    case If0(test, posBody, negBody) =>
      if (interp(test) == Num(0)) interp(posBody)
      else interp(negBody)
    case App(funExpr, argExpr) =>
      interp(funExpr) match
        case Fun(param, body) =>
          interp(subst(body, param, interp(argExpr)))
        case _ => sys.error("Can only handle function expressions")
    case Fun(arg, body) => expr

  given Conversion[String, RCFLAE] with
    def apply(s: String): RCFLAE = Id(s)
  given Conversion[Int, RCFLAE] with
    def apply(n: Int): RCFLAE = Num(n)
