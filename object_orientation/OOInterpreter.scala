package object_orientation

object OOInterpreter:

  sealed abstract class Expr
  case class Id(x: String) extends Expr
  case class New(cl: String, fvals: List[Expr]) extends Expr
  case class FAcc(o: Expr, field: String) extends Expr
  case class Invoke(o: Expr, method: String, args: List[Expr]) extends Expr

  // Object are the only values
  sealed abstract class Value
  case class Object(cl: String, fvals: List[Value]) extends Value

  case class Class(fields: List[String], methods: Map[String, Method])
  case class Method(params: List[String], body: Expr)

  type Env = Map[String, Value]
  type ClassTable = Map[String, Class]

  def interp(e: Expr, env: Env, ct: ClassTable): Value = e match
    case Id(x) => env(x)
    case New(cl, fexps) =>
      val fvals = fexps map (e => interp(e, env, ct))
      Object(cl, fvals)
    case FAcc(oexp, field) =>
      val Object(cl, fvals) = interp(oexp, env, ct)
      val Class(fnames, methods) = ct(cl)
      val findex = fnames.indexOf(field)
      fvals(findex)
    case Invoke(oexp, method, args) =>
      val rcv @ Object(cl, fvals) = interp(oexp, env, ct)
      val Class(fnames, methods) = ct(cl)
      val Method(params, body) = methods(method)

      val argVals = args map (e => interp(e, env, ct))
      val paramEnv = Map() ++ (params zip argVals)
      val envInvoke = paramEnv + ("this" -> rcv)

      interp(body, envInvoke, ct)
