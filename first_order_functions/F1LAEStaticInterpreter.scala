package first_order_functions

object F1LAEStaticInterpreter:

  sealed abstract class F1LAE
  case class Num(n: Int) extends F1LAE
  case class Add(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Sub(lhs: F1LAE, rhs: F1LAE) extends F1LAE
  case class Let(name: String, namedExpr: F1LAE, body: F1LAE) extends F1LAE
  case class Id(name: String) extends F1LAE

  case class App(funName: String, arg: F1LAE) extends F1LAE
  case class FunDef(argName: String, body: F1LAE)

  type FunDefs = Map[String, FunDef]
  type Emv = Map[String, Int]

  def interp(expr: F1LAE, funDefs: FunDefs, env: Emv): Int = expr match
    case Num(n)        => n
    case Add(lhs, rhs) => interp(lhs, funDefs, env) + interp(rhs, funDefs, env)
    case Sub(lhs, rhs) => interp(lhs, funDefs, env) - interp(rhs, funDefs, env)
    case Let(boundId, namedExpr, boundBody) =>
      val newEmv = env + (boundId -> interp(namedExpr, funDefs, env))
      interp(boundBody, funDefs, newEmv)
    case Id(name) => env(name)
    case App(funName, argExpr) =>
      funDefs(funName) match
        case FunDef(argName, body) =>
          val funEmv = Map(argName -> interp(argExpr, funDefs, env))
          interp(body, funDefs, funEmv)

  given Conversion[String, F1LAE] with
    def apply(s: String): F1LAE = Id(s)
  given Conversion[Int, F1LAE] with
    def apply(n: Int): F1LAE = Num(n)
