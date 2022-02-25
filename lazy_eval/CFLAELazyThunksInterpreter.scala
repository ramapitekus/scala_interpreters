// This file contains an interpreter for CFLAE/L with caching/thunks.
package lazy_eval

object CFLAELazyThunksInterpreter:

  sealed abstract class CFLAE
  case class Num(n: Int) extends CFLAE
  case class Add(lhs: CFLAE, rhs: CFLAE) extends CFLAE
  case class Sub(lhs: CFLAE, rhs: CFLAE) extends CFLAE
  case class Let(name: String, namedExpr: CFLAE, body: CFLAE) extends CFLAE
  case class Id(name: String) extends CFLAE
  case class Fun(param: String, body: CFLAE) extends CFLAE
  case class App(funExpr: CFLAE, argExpr: CFLAE) extends CFLAE
  case class If0(test: CFLAE, posBody: CFLAE, negBody: CFLAE) extends CFLAE

  type Env = Map[String, Value]

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  // case class FClosure(param: String, body: CFLAE, env: Env) extends Value
  case class EClosure(expr: CFLAE, env: Env) extends Value

  def interp(expr: CFLAE, env: Env = Map()): Value = expr match
    case Num(n) => NumV(n)
    case Add(lhs, rhs) =>
      val lhsV = strict(interp(lhs, env))
      val rhsV = strict(interp(rhs, env))
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _                    => sys.error("can only add numbers, but got: " + (lhsV, rhsV))
    case Sub(lhs, rhs) =>
      val lhsV = strict(interp(lhs, env))
      val rhsV = strict(interp(rhs, env))
      (lhsV, rhsV) match
        case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
        case _ => sys.error("can only subtract numbers, but got: " + (lhsV, rhsV))
    case Let(boundId, namedExpr, boundBody) =>
      interp(boundBody, env + (boundId -> EClosure(namedExpr, env)))
    case Id(name)       => strict(env(name))
    case Fun(arg, body) => EClosure(expr, env)
    case If0(testExpr, thenExpr, elseExpr) =>
      val testV = strict(interp(testExpr, env))
      testV match
        case NumV(n) =>
          if n == 0 then interp(thenExpr, env)
          else interp(elseExpr, env)
        case _ => sys.error("can only test numbers, but got: " + testV)
    case App(funExpr, argExpr) =>
      val funV = strict(interp(funExpr, env))
      funV match
        case EClosure(clExpr, clEnv) =>
          clExpr match
            case Fun(fParam, fBody) => interp(fBody, clEnv + (fParam -> EClosure(argExpr, env)))
            case _                  => sys.error("expected function expression, but got: " + clExpr)
        case _ => sys.error("expected closure for application, but got: " + funV)

  def strict(value: Value): Value = value match
    case EClosure(expr, env) =>
      expr match
        case Fun(arg, body) => value
        case _              => strict(interp(expr, env))
    case _ => value

  given Conversion[String, CFLAE] with
    def apply(s: String): CFLAE = Id(s)
  given Conversion[Int, CFLAE] with
    def apply(n: Int): CFLAE = Num(n)
