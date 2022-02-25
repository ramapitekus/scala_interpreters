package types

object PreliminaryTypes extends App:

  sealed abstract class Exp0
  case class Num0(n: Int) extends Exp0
  case class Add0(lhs: Exp0, rhs: Exp0) extends Exp0
  case class App0(funExpr: Exp0, arg: Exp0) extends Exp0
  case class Fun0(param: String, body: Exp0) extends Exp0
  case class Id0(id: String) extends Exp0

  sealed abstract class Type
  case class TNum() extends Type
  case class TFun(t1: Type, t2: Type) extends Type

  def typeOf0(e: Exp0): Type = e match

    case Num0(n) => TNum()

    case Add0(lhs, rhs)
        if typeOf0(lhs) == TNum() &&
          typeOf0(rhs) == TNum() =>
      TNum()

    case App0(e1, e2) =>
      val (t1, t2) = typeOf0(e1) match
        case TFun(t1, t2) => (t1, t2)
      val targ = typeOf0(e2)
      if t1 == targ then t2
      else sys.error("Undefined type for " + App0(e1, e2))

    // Q: what should we use as argument type?
    // A: require type annotation by the user
    case Fun0(param, body) => TFun(???, typeOf0(body))

    // Q: what is the type of a variable reference? How to detect unbound variables?
    // A: need to keep track of bindings by simulating environment statically.
    case Id0(x) => ???
