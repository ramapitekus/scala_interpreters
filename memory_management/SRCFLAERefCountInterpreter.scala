// This file contains an interpreter for SCFLAE with recursive first-class functions,
// conditionals, mutable boxes, variables and sequencing.
package memory_management

object SRCFLAERefCountInterpreter:

  sealed abstract class SRCFLAE
  case class Num(n: Int) extends SRCFLAE
  case class Add(lhs: SRCFLAE, rhs: SRCFLAE) extends SRCFLAE
  case class Mult(lhs: SRCFLAE, rhs: SRCFLAE) extends SRCFLAE
  case class Let(name: String, namedExpr: SRCFLAE, body: SRCFLAE) extends SRCFLAE
  case class Id(name: String) extends SRCFLAE
  case class If0(test: SRCFLAE, posBody: SRCFLAE, negBody: SRCFLAE) extends SRCFLAE
  case class Fun(param: String, body: SRCFLAE) extends SRCFLAE
  case class Rec(name: String, namedExpr: SRCFLAE, body: SRCFLAE) extends SRCFLAE
  case class App(funExpr: SRCFLAE, argExpr: SRCFLAE) extends SRCFLAE
  case class Seqn(e1: SRCFLAE, e2: SRCFLAE) extends SRCFLAE
  case class SetId(id: String, valueExpr: SRCFLAE) extends SRCFLAE
  case class NewBox(valExpr: SRCFLAE) extends SRCFLAE
  case class SetBox(boxExpr: SRCFLAE, valueExpr: SRCFLAE) extends SRCFLAE
  case class OpenBox(boxExpr: SRCFLAE) extends SRCFLAE

  type Location = Int
  type Env = Map[String, Location]

  sealed abstract class Val:
    var refCount: Int = 1
  case class NumV(n: Int) extends Val
  case class Closure(param: String, body: SRCFLAE, env: Env) extends Val
  case class Box(location: Location) extends Val

  /*
   * Dereference value.
   * If there was only one reference,
   *  recursively dereference nested values.
   */
  def deref(v: Val, store: Store[Val]): Store[Val] =
    v.refCount = v.refCount - 1
    if v.refCount <= 0 then destroy(v, store)
    else store
  def deref(loc: Location, store: Store[Val]): Store[Val] =
    val v = store.lookup(loc)
    val st = deref(v, store)
    if v.refCount <= 0 then st.free(loc)
    else st

  /*
   * Mark a new reference to value.
   */
  def enref(v: Val): Val =
    v.refCount = v.refCount + 1
    v

  /*
   * Dereference nested locations of a value.
   */
  def destroy(v: Val, store: Store[Val]): Store[Val] =
    v match
      case NumV(_)  => store
      case Box(loc) => deref(loc, store)
      case Closure(x, body, env) =>
        env.values.foldRight(store)((loc, store2) => deref(loc, store2))

  def interp(
      expr: SRCFLAE,
      stack: List[Env] = List(Map()),
      store: Store[Val] = new NoGCStore[Val](100)
  ): (Val, Store[Val]) = expr match
    case Num(n) => (NumV(n), store)
    // deref the intermediate values lhsv and rhsv
    case Add(lhs, rhs) =>
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match
        case (NumV(n1), _) =>
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match
            case (NumV(n2), _) =>
              val s3 = deref(lhsv, deref(rhsv, s2))
              (NumV(n1 + n2), s3)
            case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))
        case _ => sys.error("can only add numbers, but got: '%s' as left hand side".format(lhsv))
    case Mult(lhs, rhs) =>
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match
        case (NumV(n1), _) =>
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match
            case (NumV(n2), _) =>
              val s3 = deref(lhsv, deref(rhsv, s2))
              (NumV(n1 * n2), s3)
            case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))
        case _ => sys.error("can only add numbers, but got: '%s' as left hand side".format(lhsv))
    // refCount of namedVal does not change since we deref the intermediate result
    // and immediately enref the value of newLoc
    case Let(boundId, namedExpr, boundBody) =>
      val (namedVal, s1) = interp(namedExpr, stack, store)
      val (newLoc, s2) = s1.malloc(stack, namedVal)
//      enref(namedVal)
//      val s3 = deref(namedVal, s2)
      val (v, s3) = interp(boundBody, stack.head + (boundId -> newLoc) :: stack.tail, s2)
      val s4 = deref(newLoc, s3)
      (v, s4)
    case Id(name) =>
      val v = store.lookup(stack.head(name))
      enref(v)
      (v, store)
    case Fun(arg, body) =>
      val env = stack.head
      env.map(kv => enref(store.lookup(kv._2)))
      (Closure(arg, body, env), store)
    case If0(testExpr, thenExpr, elseExpr) =>
      val (testV, s1) = interp(testExpr, stack, store)
      testV match
        case NumV(n) =>
          val s2 = deref(testV, s1)
          if n == 0 then interp(thenExpr, stack, s2)
          else interp(elseExpr, stack, s2)
        case _ => sys.error("can only test numbers, but got: " + testV)
    /** In our stateful language, we do not require mutation from the host language to implement
      * cyclic environments.
      */
    case Rec(boundId, namedExpr, boundBody) =>
      val (newLoc, s2) = store.malloc(stack, NumV(0))
      val extStack = stack.head + (boundId -> newLoc) :: stack.tail
      val (namedVal, bodyStore) = interp(namedExpr, extStack, s2)

      // deref and enref because of overwrite in `update`
      val st = deref(bodyStore.lookup(newLoc), bodyStore)
      enref(namedVal)
      val st2 = st.update(newLoc, namedVal)

      interp(boundBody, extStack, st2)
    case App(funExpr, argExpr) =>
      val (funV, funStore) = interp(funExpr, stack, store)
      val funStore2 = deref(funV, funStore)
      val (argV, argStore) = interp(argExpr, stack, funStore2)
      funV match
        case Closure(fParam, fBody, fEnv) =>
          val (newLoc, resStore) = argStore.malloc(stack, argV)
//          enref(argV)
//          val resStore2 = deref(argV, resStore)
          val (v, resStore2) = interp(fBody, fEnv + (fParam -> newLoc) :: stack, resStore)
          val resStore3 = deref(newLoc, resStore2)
          (v, resStore3)
        case _ => sys.error("can only apply functions, but got: " + funV)
    case Seqn(e1, e2) =>
      val (v1, s1) = interp(e1, stack, store)
      val s2 = deref(v1, s1)
      interp(e2, stack, s2)
    case NewBox(boxExpr) =>
      val (boxV, boxStore) = interp(boxExpr, stack, store)
      val (newLoc, resStore) = boxStore.malloc(stack, boxV)
      (Box(newLoc), resStore)
    case SetBox(boxExpr, valueExpr) =>
      val (boxV, s1) = interp(boxExpr, stack, store)
      val (value, s2) = interp(valueExpr, stack, s1)
      boxV match
        case Box(loc) =>
          val s4 = deref(loc, s2)
          enref(value)
          (value, s4.update(loc, value))
        case _ => sys.error("can only set to boxes, but got: " + boxV)
    case OpenBox(boxExpr) =>
      val (boxV, s1) = interp(boxExpr, stack, store)
      boxV match
        case Box(loc) =>
          val v = s1.lookup(loc)
          enref(v)
          val s2 = deref(boxV, s1)
          (v, s2)
        case _ => sys.error("can only open boxes, but got: " + boxV)
    case SetId(id, valExpr) =>
      val (value, s1) = interp(valExpr, stack, store)
      val loc = stack.head(id)
      enref(value)
      (value, deref(loc, s1).update(loc, value))

  given Conversion[String, SRCFLAE] with
    def apply(s: String): SRCFLAE = Id(s)
  given Conversion[Int, SRCFLAE] with
    def apply(n: Int): SRCFLAE = Num(n)

  def whatDoesThisDo(n: Int): SRCFLAE =
    var v: SRCFLAE = Num(17)
    for (i <- 0 to n)
      v = Seqn(NewBox(i), v)
    v

  @main def runSRCFLAERefCountInterpreter(iterations: Int, storeSize: Int) =
    val store = new NoGCStore[Val](storeSize)
    interp(whatDoesThisDo(iterations), store = store)
    println(s"all ok, final store size is ${store.nextFreeAddr - store.freed.size}")
