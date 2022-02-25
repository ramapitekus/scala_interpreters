package first_class_functions

object FLAEImmediateSubstitutionInterpreter:

  sealed abstract class FLAE
  case class Num(n: Int) extends FLAE
  case class Add(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Sub(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Let(name: String, namedExpr: FLAE, body: FLAE) extends FLAE
  case class Id(name: String) extends FLAE
  case class Fun(param: String, body: FLAE) extends FLAE
  case class App(funExp: FLAE, arg: FLAE) extends FLAE

  def subst(expr: FLAE, substId: String, value: FLAE): FLAE = expr match
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
    // Fun("x", Fun("x", x + 1)) 1 2
    // -> Fun("x", x + 1) 2
    // -> 3
    case Fun(param, body) =>
      if param == substId then Fun(param, body)
      else Fun(param, subst(body, substId, value))
    case App(funExpr, argExpr) =>
      App(subst(funExpr, substId, value), subst(argExpr, substId, value))

  abstract class Value
  case class VNum(n: Int) extends Value
  case class VFun(param: String, body: FLAE) extends Value

  def interp(expr: FLAE): Value = expr match
    case Num(n) => VNum(n)
    case Add(lhs, rhs) =>
      interp(lhs) match
        case VNum(n1) =>
          interp(rhs) match
            case VNum(n2) => VNum(n1 + n2)
            case v2       => sys.error(s"Expected numeric value but got $v2")
        case v1 => sys.error(s"Expected numeric value but got $v1")
    case Sub(lhs, rhs) =>
      interp(lhs) match
        case VNum(n1) =>
          interp(rhs) match
            case VNum(n2) => VNum(n1 - n2)
            case v2       => sys.error(s"Expected numeric value but got $v2")
        case v1 => sys.error(s"Expected numeric value but got $v1")
    case Let(boundId, namedExpr, boundBody) =>
      val body = subst(boundBody, boundId, namedExpr)
      interp(body)
    case Id(name)         => sys.error("found unbound id " + name)
    case Fun(param, body) => VFun(param, body)
    // funExpr can be an App:
    //   App(App(Fun("x", "x"), Fun("y", "y")), 5)
    case App(funExpr, argExpr) =>
      interp(funExpr) match
        case VFun(param, body) => interp(subst(body, param, argExpr))
        case v1                => sys.error(s"Expected function value but got $v1")

  given Conversion[String, FLAE] with
    def apply(s: String): FLAE = Id(s)
  given Conversion[Int, FLAE] with
    def apply(n: Int): FLAE = Num(n)
