package reactive_programming

object ReactivePullNoCacheInterpreter:

  sealed abstract class RSCFLAE
  case class Num(n: Int) extends RSCFLAE
  case class Add(lhs: RSCFLAE, rhs: RSCFLAE) extends RSCFLAE
  case class Let(name: String, namedExpr: RSCFLAE, body: RSCFLAE) extends RSCFLAE
  case class Id(name: String) extends RSCFLAE
  case class If0(test: RSCFLAE, posBody: RSCFLAE, negBody: RSCFLAE) extends RSCFLAE
  case class Fun(param: String, body: RSCFLAE) extends RSCFLAE
  case class App(funExpr: RSCFLAE, argExpr: RSCFLAE) extends RSCFLAE
  case class Seqn(e1: RSCFLAE, e2: RSCFLAE) extends RSCFLAE
  case class NewVar(valExpr: RSCFLAE) extends RSCFLAE
  case class SetVar(varExpr: RSCFLAE, valueExpr: RSCFLAE) extends RSCFLAE
  case class CurrVal(varExpr: RSCFLAE) extends RSCFLAE

  given Conversion[String, RSCFLAE] with
    def apply(s: String): RSCFLAE = Id(s)
  given Conversion[Int, RSCFLAE] with
    def apply(n: Int): RSCFLAE = Num(n)

  type Location = Int
  var _currentLocation = 0
  def nextLocation: Location =
    _currentLocation += 1
    _currentLocation

  type Env = Map[String, Location]
  type Store = Map[Location, Val]

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: String, body: RSCFLAE, env: Env) extends Val
  case class Var(location: Location) extends Val

  def eval(expr: RSCFLAE) = interp(expr, Map(), Map())

  def interp(expr: RSCFLAE, env: Env = Map(), store: Store = Map()): (Val, Store) = expr match
    case Num(n) => (NumV(n), store)
    case Add(lhs, rhs) =>
      val (lhsv, s1) = interp(lhs, env, store)
      val (rhsv, s2) = interp(rhs, env, s1)
      // When adding vars, don't use the interpreted result but simply construct a new var around them
      (lhsv, rhsv) match
        case (Var(_), Var(_))     => interp(NewVar(Add(CurrVal(lhs), CurrVal(rhs))), env, store)
        case (Var(_), _)          => interp(NewVar(Add(CurrVal(lhs), rhs)), env, store)
        case (_, Var(_))          => interp(NewVar(Add(lhs, CurrVal(rhs))), env, store)
        case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), s2)
        case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))
    case Let(boundId, namedExpr, boundBody) =>
      val (namedVal, s1) = interp(namedExpr, env, store)
      val newLoc = nextLocation
      interp(boundBody, env + (boundId -> newLoc), s1 + (newLoc -> namedVal))
    case Id(name)       => (store(env(name)), store)
    case Fun(arg, body) => (Closure(arg, body, env), store)
    case If0(testExpr, thenExpr, elseExpr) =>
      val (testV, s1) = interp(testExpr, env, store)
      testV match
        case Var(_) =>
          val (thenV, s2) = interp(thenExpr, env, s1)
          thenV match
            case Var(_) =>
              val (elseV, s3) = interp(elseExpr, env, s2)
              elseV match
                case Var(_) =>
                  interp(
                    NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), CurrVal(elseExpr))),
                    env,
                    s3
                  )
                case _ =>
                  interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), elseExpr)), env, s2)
            case _ =>
              val (elseV, s3) = interp(elseExpr, env, s1)
              elseV match
                case Var(_) =>
                  interp(NewVar(If0(CurrVal(testExpr), thenExpr, CurrVal(elseExpr))), env, s3)
                case _ => interp(NewVar(If0(CurrVal(testExpr), thenExpr, elseExpr)), env, s1)
        case NumV(n) =>
          if n == 0 then interp(thenExpr, env, s1)
          else interp(elseExpr, env, s1)
        case _ => sys.error("can only test numbers, but got: " + testV)
    case App(funExpr, argExpr) =>
      val (funV, s1) = interp(funExpr, env, store)
      val (argV, argStore) = interp(argExpr, env, s1)
      funV match
        case Var(_) => interp(NewVar(App(CurrVal(funExpr), argExpr)), env, store)
        case Closure(fParam, fBody, fEnv) =>
          val newLoc = nextLocation
          interp(fBody, fEnv + (fParam -> newLoc), argStore + (newLoc -> argV))
        case _ => sys.error("can only apply functions, but got: " + funV)
    case Seqn(e1, e2) =>
      val (v1, s1) = interp(e1, env, store)
      interp(e2, env, s1)
    case NewVar(varExpr) =>
      // Create closure around varExpr ...
      val varBody = Closure("_", varExpr, env)
      val newLoc = nextLocation
      // ... and store it at the location saved in Var
      (Var(newLoc), store + (newLoc -> varBody))
    case SetVar(varExpr, valueExpr) =>
      val (varV, s1) = interp(varExpr, env, store)
      val varBody = Closure("_", valueExpr, env)
      varV match
        case Var(loc) => (varV, s1 + (loc -> varBody))
        case _        => sys.error("can only set to vars, but got: " + varV)
    case CurrVal(valExpr) =>
      val (valV, s1) = interp(valExpr, env, store)
      valV match
        // For Vars, CurrVal extracts the current content of the var ...
        case Var(loc) =>
          s1(loc) match
            case Closure(_, body, vEnv) =>
              // ... and executes it
              interp(body, vEnv, s1)
            case _ => sys.error("invalid content of var: " + s1(loc))
        // For all other values, it just returns the value itself
        case x => (x, s1)
