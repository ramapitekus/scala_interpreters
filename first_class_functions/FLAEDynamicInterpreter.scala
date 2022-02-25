package first_class_functions

object FLAEDynamicInterpreter:

  sealed abstract class FLAE
  case class Num(n: Int) extends FLAE
  case class Add(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Sub(lhs: FLAE, rhs: FLAE) extends FLAE
  case class Let(name: String, namedExpr: FLAE, body: FLAE) extends FLAE
  case class Id(name: String) extends FLAE
  case class Fun(param: String, body: FLAE) extends FLAE
  case class App(funExpr: FLAE, arg: FLAE) extends FLAE

  type Env = Map[String, FLAE]

  def interp(expr: FLAE, env: Env = Map()): FLAE = expr match
    case Num(n) => expr
    case Add(lhs, rhs) =>
      val lhsv = interp(lhs, env)
      lhsv match
        case Num(n) =>
          val rhsv = interp(rhs, env)
          rhsv match
            case Num(m) => Num(n + m)
            case _      => sys.error("Can only add numbers")
        case _ => sys.error("Can only add numbers")
    case Sub(lhs, rhs) =>
      val lhsv = interp(lhs, env)
      lhsv match
        case Num(n) =>
          val rhsv = interp(rhs, env)
          rhsv match
            case Num(m) => Num(n - m)
            case _      => sys.error("Can only subtract numbers")
        case _ => sys.error("Can only subtract numbers")
    case Let(boundId, namedExpr, body) =>
      interp(body, env + (boundId -> interp(namedExpr, env)))
    case Id(name) => env(name)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match
        case Fun(param, body) =>
          interp(body, env + (param -> interp(argExpr, env)))
        case _ => sys.error("Can only handle function expressions")
    case Fun(arg, body) => expr

  given Conversion[String, FLAE] with
    def apply(s: String): FLAE = Id(s)
  given Conversion[Int, FLAE] with
    def apply(n: Int): FLAE = Num(n)
