package first_class_functions

object CFLAEDynamicInterpreter:

  sealed abstract class Expr
  case class Num(n: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr
  case class Mult(lhs: Expr, rhs: Expr) extends Expr
  case class Let(name: String, namedExpr: Expr, body: Expr) extends Expr
  case class Id(name: String) extends Expr
  case class If0(test: Expr, posBody: Expr, negBody: Expr) extends Expr
  case class Fun(param: String, body: Expr) extends Expr
  case class App(funExpr: Expr, arg: Expr) extends Expr

  type Env = Map[String, Expr]

  def interp(expr: Expr, env: Env = Map()): Expr = expr match
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
    case Mult(lhs, rhs) =>
      val lhsV = interp(lhs, env)
      val rhsV = interp(rhs, env)
      (lhsV, rhsV) match
        case (Num(n1), Num(n2)) => Num(n1 * n2)
        case _                  => sys.error("can only multiply numbers, but got: " + (lhsV, rhsV))
    case Let(boundId, namedExpr, body) =>
      interp(body, env + (boundId -> interp(namedExpr, env)))
    case Id(name) => env(name)
    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = interp(testExpr, env)
      testV match
        case Num(n) =>
          if n == 0 then interp(thenExpr, env)
          else interp(elseExpr, env)
        case _ => sys.error("can only test numbers, but got: " + testV)
    case App(funExpr, argExpr) =>
      interp(funExpr, env) match
        case Fun(param, body) => interp(body, env + (param -> interp(argExpr, env)))
        case _                => sys.error("Can only handle function expressions")
    case Fun(arg, body) => expr

  given Conversion[String, Expr] with
    def apply(s: String): Expr = Id(s)
  given Conversion[Int, Expr] with
    def apply(n: Int): Expr = Num(n)
