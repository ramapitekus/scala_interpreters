package reactive_programming

import java.util.Date

object ReactivePushStaticInterpreter:

  sealed abstract class RSCFLAE
  case class Num(n: Int) extends RSCFLAE
  case class Add(lhs: RSCFLAE, rhs: RSCFLAE) extends RSCFLAE
  case class Sub(lhs: RSCFLAE, rhs: RSCFLAE) extends RSCFLAE
  case class Let(name: String, namedExpr: RSCFLAE, body: RSCFLAE) extends RSCFLAE
  case class Id(name: String) extends RSCFLAE
  case class If0(test: RSCFLAE, posBody: RSCFLAE, negBody: RSCFLAE) extends RSCFLAE
  case class Fun(param: String, body: RSCFLAE) extends RSCFLAE
  case class App(funExpr: RSCFLAE, argExpr: RSCFLAE) extends RSCFLAE
  case class Seqn(e1: RSCFLAE, e2: RSCFLAE) extends RSCFLAE
  case class NewVar(valExpr: RSCFLAE) extends RSCFLAE
  case class SetVar(varExpr: RSCFLAE, valueExpr: RSCFLAE) extends RSCFLAE
  case class CurrVal(varExpr: RSCFLAE) extends RSCFLAE
  case class Time() extends RSCFLAE
  case class Wait(millis: Int) extends RSCFLAE

  given Conversion[String, RSCFLAE] with
    def apply(s: String): RSCFLAE = Id(s)
  given Conversion[Int, RSCFLAE] with
    def apply(n: Int): RSCFLAE = Num(n)

  type Location = Int
  // First two entries are reserved for time and its dependencies
  val timeLoc = 0
  val timeDepLoc = 1

  var _currentLocation = timeDepLoc
  def nextLocation: Location =
    _currentLocation += 1
    _currentLocation

  type Env = Map[String, Location]
  type Store = Map[Location, Val]

  sealed abstract class Val
  case class NumV(n: Long) extends Val
  case class Closure(param: String, body: RSCFLAE, env: Env) extends Val
  case class Var(valLoc: Location, exprLoc: Location, depLoc: Location) extends Val
  case class VarDependencies(dependencies: List[Val] = List()) extends Val

  // Traverses the dependency tree in post-order and returns a reversed linearization that allows glitch-free updating
  def orderDependencies(dependencies: List[Val], store: Store): List[Val] =
    orderDependencies(dependencies, store, Set())._1.reverse

  def orderDependencies(
      dependencies: List[Val],
      store: Store,
      visited: Set[Val]
  ): (List[Val], Set[Val]) =
    dependencies.foldLeft((List[Val](), visited))((prev, nextDep) => {
      if prev._2.contains(nextDep) then prev
      else
        (nextDep match
          case Var(_, _, depLoc) =>
            store(depLoc) match
              case VarDependencies(dep) =>
                val (newList, newVisited) = orderDependencies(dep, store, prev._2 + nextDep)
                (prev._1 ++ newList :+ nextDep, prev._2 ++ newVisited)
              case _ => sys.error("Invalid dependency list: " + depLoc)
          case _ => sys.error("Can only order dependencies for vars, but got: " + nextDep)
      )
    })

  // Adds a dependency link between two vars
  def addDependency(fromVar: Val, toVar: Val, store: Store): Store =
    fromVar match
      case Var(_, _, depLoc) =>
        store(depLoc) match
          case VarDependencies(depList) => store + (depLoc -> VarDependencies(depList :+ toVar))
          case _                        => sys.error("Invalid dependency list: " + depLoc)
      case _ => sys.error("Can only add dependencies to vars, but got: " + fromVar)

  // Updates the dependencies stored at the given location in the store
  def updateDependencies(depLoc: Location, store: Store): Store =
    store(depLoc) match
      case VarDependencies(dependencies) =>
        val updateSchedule = orderDependencies(dependencies, store)
        val sNew = updateSchedule.foldLeft(store)((sPrev, dep) =>
          dep match
            case Var(depValLoc, depExprLoc, depDepLoc) =>
              sPrev(depExprLoc) match
                case Closure(_, depExprBody, depExprEnv) =>
                  val (depValNew, depSNew) = interp(depExprBody, depExprEnv, sPrev)
                  depSNew + (depValLoc -> depValNew)
                case _ => sys.error("Encountered non-closure value as var expression: " + dep)
            case _ => sys.error("Encountered non-var value in dependency list: " + dep)
        )
        sNew
      case _ => sys.error("Invalid dependency list: " + depLoc)

  def updateTime(store: Store): Store =
    val s1 = store +
      (timeLoc -> NumV(new Date().getTime.toInt)) +
      (timeDepLoc -> store.getOrElse(timeDepLoc, VarDependencies()))
    updateDependencies(timeDepLoc, s1)

  def interp(expr: RSCFLAE, env: Env = Map(), storeOld: Store = Map()): (Val, Store) =
    val store = updateTime(storeOld)

    expr match
      case Num(n) => (NumV(n), store)

      case Add(lhs, rhs) =>
        val (lhsv, s1) = interp(lhs, env, store)
        val (rhsv, s2) = interp(rhs, env, s1)

        (lhsv, rhsv) match
          case (Var(_, _, _), Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Add(CurrVal(lhs), CurrVal(rhs))), env, s2)
            val s4 = addDependency(lhsv, newVar, addDependency(rhsv, newVar, s3))
            (newVar, s4)
          case (Var(_, _, _), _) =>
            val (newVar, s3) = interp(NewVar(Add(CurrVal(lhs), rhs)), env, s2)
            val s4 = addDependency(lhsv, newVar, s3)
            (newVar, s4)
          case (_, Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Add(lhs, CurrVal(rhs))), env, s2)
            val s4 = addDependency(rhsv, newVar, s3)
            (newVar, s4)
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), s2)
          case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))

      case Sub(lhs, rhs) =>
        val (lhsv, s1) = interp(lhs, env, store)
        val (rhsv, s2) = interp(rhs, env, s1)

        (lhsv, rhsv) match
          case (Var(_, _, _), Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Sub(CurrVal(lhs), CurrVal(rhs))), env, s2)
            val s4 = addDependency(lhsv, newVar, addDependency(rhsv, newVar, s3))
            (newVar, s4)
          case (Var(_, _, _), _) =>
            val (newVar, s3) = interp(NewVar(Sub(CurrVal(lhs), rhs)), env, s2)
            val s4 = addDependency(lhsv, newVar, s3)
            (newVar, s4)
          case (_, Var(_, _, _)) =>
            val (newVar, s3) = interp(NewVar(Sub(lhs, CurrVal(rhs))), env, s2)
            val s4 = addDependency(rhsv, newVar, s3)
            (newVar, s4)
          case (NumV(n1), NumV(n2)) => (NumV(n1 - n2), s2)
          case _ => sys.error("can only subtract numbers, but got: %s and %s".format(lhsv, rhsv))

      case Let(boundId, namedExpr, boundBody) =>
        val (namedVal, s1) = interp(namedExpr, env, store)
        val newLoc = nextLocation
        interp(boundBody, env + (boundId -> newLoc), s1 + (newLoc -> namedVal))

      case Id(name) => (store(env(name)), store)

      case Fun(arg, body) => (Closure(arg, body, env), store)

      case If0(testExpr, thenExpr, elseExpr) =>
        val (testV, s1) = interp(testExpr, env, store)

        testV match
          case Var(_, _, _) =>
            val (thenV, s2) = interp(thenExpr, env, s1)
            thenV match
              case Var(_, _, _) =>
                val (elseV, s3) = interp(elseExpr, env, s2)
                elseV match
                  case Var(_, _, _) =>
                    val (newVar, s4) = interp(
                      NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), CurrVal(elseExpr))),
                      env,
                      s3
                    )
                    val s5 = addDependency(
                      testV,
                      newVar,
                      addDependency(thenV, newVar, addDependency(elseV, newVar, s4))
                    )
                    (newVar, s5)
                  case _ =>
                    val (newVar, s4) =
                      interp(NewVar(If0(CurrVal(testExpr), CurrVal(thenExpr), elseExpr)), env, s2)
                    val s5 = addDependency(testV, newVar, addDependency(thenV, newVar, s4))
                    (newVar, s5)
              case _ =>
                val (elseV, s3) = interp(elseExpr, env, s1)
                elseV match
                  case Var(_, _, _) =>
                    val (newVar, s4) =
                      interp(NewVar(If0(CurrVal(testExpr), thenExpr, CurrVal(elseExpr))), env, s3)
                    val s5 = addDependency(testV, newVar, addDependency(elseV, newVar, s4))
                    (newVar, s5)
                  case _ =>
                    val (newVar, s4) =
                      interp(NewVar(If0(CurrVal(testExpr), thenExpr, elseExpr)), env, s1)
                    val s5 = addDependency(testV, newVar, s4)
                    (newVar, s5)
          case NumV(n) =>
            if n == 0 then interp(thenExpr, env, s1)
            else interp(elseExpr, env, s1)
          case _ => sys.error("can only test numbers, but got: " + testV)

      case App(funExpr, argExpr) =>
        val (funV, s1) = interp(funExpr, env, store)

        funV match
          case Var(_, _, _) =>
            val (newVar, s2) = interp(NewVar(App(CurrVal(funExpr), argExpr)), env, s1)
            val s3 = addDependency(funV, newVar, s2)
            (newVar, s3)
          case Closure(fParam, fBody, fEnv) =>
            val (argV, s2) = interp(argExpr, env, s1)
            val newLoc = nextLocation
            interp(fBody, fEnv + (fParam -> newLoc), s2 + (newLoc -> argV))
          case _ => sys.error("can only apply functions, but got: " + funV)

      case Seqn(e1, e2) =>
        val (v1, s1) = interp(e1, env, store)
        interp(e2, env, s1)

      case NewVar(varExpr) =>
        // Compute the initial value of the var
        val (varVal, _) = interp(varExpr, env, store)
        val valLoc = nextLocation
        // And save the uninterpreted expression as closure for later updates
        val varBody = Closure("_", varExpr, env)
        val bodyLoc = nextLocation
        // Location where later dependencies are stored (initally empty)
        val depLoc = nextLocation

        (
          Var(valLoc, bodyLoc, depLoc),
          store + (valLoc -> varVal) + (bodyLoc -> varBody) + (depLoc -> VarDependencies())
        )

      case SetVar(varExpr, valueExpr) =>
        val (varV, s1) = interp(varExpr, env, store)
        // Compute new values for the var ...
        val (valueV, s2) = interp(valueExpr, env, s1)
        val varBody = Closure("_", valueExpr, env)

        varV match
          case Var(valLoc, exprLoc, depLoc) =>
            // ... update the var in the store
            val s3 = s2 + (valLoc -> valueV) + (exprLoc -> varBody)
            // ... and update all dependent vars
            val s4 = updateDependencies(depLoc, s3)
            (varV, s4)
          case _ => sys.error("can only set to vars, but got: " + varV)

      case CurrVal(valExpr) =>
        val (valV, s1) = interp(valExpr, env, store)
        valV match
          // For Vars, CurrVal extracts the current content of the var
          case Var(valLoc, _, _) =>
            (s1(valLoc), s1)
          // For all other values, it just returns the value itself
          case x => (x, s1)

      case Time() =>
        (Var(timeLoc, timeLoc, timeDepLoc), store)

      case Wait(millis) =>
        Thread.sleep(millis)
        (NumV(millis), store)
