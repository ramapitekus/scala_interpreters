package continuations

object KCFLAEInterpreter:

  sealed abstract class KCFLAE
  case class Num(n: Int) extends KCFLAE
  case class Add(lhs: KCFLAE, rhs: KCFLAE) extends KCFLAE
  case class Sub(lhs: KCFLAE, rhs: KCFLAE) extends KCFLAE
  case class Mult(lhs: KCFLAE, rhs: KCFLAE) extends KCFLAE
  case class Let(name: String, namedE: KCFLAE, body: KCFLAE) extends KCFLAE
  case class Id(name: String) extends KCFLAE
  case class Fun(param: String, body: KCFLAE) extends KCFLAE
  case class App(funExpr: KCFLAE, arg: KCFLAE) extends KCFLAE
  case class If0(testE: KCFLAE, thenE: KCFLAE, elseE: KCFLAE) extends KCFLAE
  case class BindCC(name: String, body: KCFLAE) extends KCFLAE

  sealed abstract class Value
  case class NumV(n: Int) extends Value
  case class Closure(param: String, body: KCFLAE, env: Env) extends Value

  type Env = Map[String, Value]

  def interp[A](expr: KCFLAE, env: Env, k: (Value => A)): A =
    // Abstract over left associative number operations
    def lAssocNumOp[A](verb: String, op: (Int, Int) => Int, lhs: KCFLAE, rhs: KCFLAE) =
      interp(
        lhs,
        env,
        lv => {
          interp(
            rhs,
            env,
            (rv) =>
              (lv, rv) match
                case (NumV(n1), NumV(n2)) => k(NumV(op(n1, n2)))
                case _ => sys.error("Can only %s numbers, but got %s and %s".format(verb, lv, rv))
          )
        }
      )

    /** Continuations are first class values. They are defined inside the interp function to allow
      * type erasure over A.
      */
    case class Continuation(c: (Value => A)) extends Value
    expr match
      case Num(n)         => k(NumV(n))
      case Add(lhs, rhs)  => lAssocNumOp("add", { _ + _ }, lhs, rhs)
      case Sub(lhs, rhs)  => lAssocNumOp("subtract", { _ - _ }, lhs, rhs)
      case Mult(lhs, rhs) => lAssocNumOp("multiply", { _ * _ }, lhs, rhs)
      case Let(boundId, namedExpr, body) =>
        interp(namedExpr, env, nv => interp(body, env + (boundId -> nv), k))
      case Id(name) => k(env(name))
      case App(funExpr, argExpr) =>
        interp(
          funExpr,
          env,
          fv =>
            interp(
              argExpr,
              env,
              argV =>
                fv match
                  case Closure(param, body, funEnv) => interp(body, funEnv + (param -> argV), k)

                  case Continuation(c) => c(argV)
                  case _ =>
                    sys.error("Can only apply closures or continuations, but got %s".format(fv))
            )
        )
      case Fun(arg, body) => k(Closure(arg, body, env))
      case If0(c, t, e) =>
        interp(
          c,
          env,
          cv =>
            cv match
              case NumV(0) => interp(t, env, k)
              case _       => interp(e, env, k)
        )
      case BindCC(name, body) =>
        interp(body, env + (name -> Continuation(k)), k)

  // Little helper for easier evaluation
  def eval(e: KCFLAE): Value = interp(e, Map.empty, identity)

  given Conversion[String, KCFLAE] with
    def apply(s: String): KCFLAE = Id(s)
  given Conversion[Int, KCFLAE] with
    def apply(n: Int): KCFLAE = Num(n)
