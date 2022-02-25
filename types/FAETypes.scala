package types

object FAETypes:

  sealed abstract class FAE
  case class Num(n: Int) extends FAE
  case class Add(lhs: FAE, rhs: FAE) extends FAE
  case class App(funExpr: FAE, arg: FAE) extends FAE

  // We require users to annotate the type of function parameters.
  case class Fun(param: String, typ: Type, body: FAE) extends FAE
  case class Id(id: String) extends FAE

  sealed abstract class Type
  case class TNum() extends Type
  case class TFun(tparam: Type, treturn: Type) extends Type

  // To keep track of variable bindings, we need to simulate the environment
  // of our interpreter in the type checker. The environment maps variable
  // names to their value at run time:
  // type Val = Int // Dummy definition
  // type Env = Map[String, Val]

  // At compile time, we don't know the actual values. But we can assert what
  // there types are. That is, for each variable, we keep track of the type of
  // values that the variable can refer to:
  type Ctx = Map[String, Type]
  def bind(x: String, t: Type, ctx: Ctx) = ctx + (x -> t)

  // This structure is called a typing context. Since we want to prevent errors
  // that might occur at run time during interpretation of the program, our type
  // checker needs to simulate the actions performed by the interpreter. That is,
  // whenever the interpreter changes the environment, our type checker must
  // perform a similar change to the typing context.
  def typeOf(e: FAE, ctx: Ctx = Map()): Type = e match
    case Num(n) => TNum()
    case Add(lhs, rhs)
        if typeOf(lhs, ctx: Ctx) == TNum() &&
          typeOf(rhs, ctx: Ctx) == TNum() =>
      TNum()
    case App(funExpr, arg) =>
      val (tparam, treturn) = typeOf(funExpr, ctx) match
        case TFun(tparam, treturn) => (tparam, treturn)
        case tfun => sys.error(s"Mismatching function type. Expected $TFun but was $tfun")
      typeOf(arg, ctx: Ctx) match
        case `tparam` => treturn // arg == tparam
        case targ => sys.error("Mismatching argument type. Expected " + tparam + " but was " + targ)

    // Q: what should we use as argument type?
    // A: require type annotation by the user
    case Fun(param, typeAnnotation, body) =>
      val ctx1 = bind(param, typeAnnotation, ctx)
      TFun(typeAnnotation, typeOf(body, ctx1))

    // Q: what is the type of a variable reference? How to detect unbound variables?
    // A: need to keep track of bindings by simulating environment statically.
    case Id(x) => ctx.getOrElse(x, sys.error("Unbound variable " + x))
