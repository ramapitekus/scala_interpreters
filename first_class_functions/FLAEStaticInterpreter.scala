package first_class_functions

object FLAEStaticInterpreter:

  sealed abstract class FLAE
  case class Num(n: Int) extends FLAE
  case class Add(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Sub(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Let(name: String, namedExpr: FLAE, body: FLAE) extends FLAE
  case class Id(name: String) extends FLAE

  case class Fun(param: String, body: FLAE) extends FLAE
  case class App(funExp: FLAE, arg: FLAE) extends FLAE

  abstract class Value
  case class VNum(n: Int) extends Value
  case class VClosure(param: String, body: FLAE, env: Env) extends Value

  type Env = Map[String, Value]

  def interp(expr: FLAE, env: Env = Map()): Value = expr match
    case Num(n) => VNum(n)
    case Add(lhs, rhs) =>
      interp(lhs, env) match
        case VNum(n1) =>
          interp(rhs, env) match
            case VNum(n2) => VNum(n1 + n2)
            case v2       => sys.error(s"Expected numeric value but got $v2")
        case v1 => sys.error(s"Expected numeric value but got $v1")
    case Sub(lhs, rhs) =>
      interp(lhs, env) match
        case VNum(n1) =>
          interp(rhs, env) match
            case VNum(n2) => VNum(n1 - n2)
            case v2       => sys.error(s"Expected numeric value but got $v2")
        case v1 => sys.error(s"Expected numeric value but got $v1")
    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> interp(namedExpr, env)))
    case Id(name)         => env.getOrElse(name, sys.error(s"unbound variable $name"))
    case Fun(param, body) => VClosure(param, body, env)
    // funExpr can be an App:
    //   App(App(Fun("x", "x"), Fun("y", "y")), 5)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match
        case VClosure(param, body, funEnv) =>
          val argVal = interp(argExpr, env)
          val extendedEnv: Env = funEnv + (param -> argVal)
          interp(body, extendedEnv)
        case v1 => sys.error(s"Expected function value but got $v1")

  given Conversion[String, FLAE] with
    def apply(s: String): FLAE = Id(s)
  given Conversion[Int, FLAE] with
    def apply(n: Int): FLAE = Num(n)
