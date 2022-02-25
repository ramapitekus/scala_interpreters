// This file contains an interpreter for SCFLAE with recursive first-class functions,
// conditionals, mutable boxes, variables and sequencing.

/*
 * Based on the lecture notes for the "Programming Languages and Types"
 * course by Klaus Ostermann at the University of Marburg.
 */
package memory_management
import scala.reflect.ClassTag

/* To be able to experiment with different store and gc designs, we
 * create an interface for stores. The stack parameter in malloc is
 * needed during gc to determine the root nodes from which the
 * algorithms can start.
 */
trait Store[Val]:
  def malloc(stack: List[Map[String, Int]], v: Val): (Int, Store[Val])
  def update(index: Int, v: Val): Store[Val]
  def lookup(index: Int): Val
  def free(index: Int): Store[Val]

/* Here is one implementation of the Store interface that does not
 * perform gc. It just runs out of memory once the store is full.
 */
class NoGCStore[Val: ClassTag](var maxSize: Int) extends Store[Val]:
  val memory = new Array[Val](maxSize)
  var freed = Set[Int]()

  var nextFreeAddr: Int = 0

  def malloc(stack: List[Map[String, Int]], v: Val) =
    // There is a memory location that has been freed before
    if !freed.isEmpty then
      val next = freed.head
      freed -= next
      update(next, v)
      (next, this)
    // Need to get the next free location in the array
    else
      val x = nextFreeAddr
      if x > maxSize then sys.error("out of memory")
      nextFreeAddr += 1
      update(x, v)
      (x, this)

  def update(index: Int, v: Val) =
    memory.update(index, v)
    this

  def free(index: Int) =
    freed += index
    this

  def lookup(index: Int) = memory(index)

object SRCFLAEInterpreter:
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

  sealed abstract class Val
  case class NumV(n: Int) extends Val
  case class Closure(param: String, body: SRCFLAE, env: Env) extends Val
  case class Box(location: Location) extends Val

  /* In our interpreter, the stack of environments is only implicitly
   * available on the stack of the meta-language. To reify the call-
   * stack we need to make it explicit. We do so by constructing the
   * stack explicitly and passing it as parameter. The first element
   * of the stack is the current environment; the rest is only needed
   * for gc.
   */
  def interp(
      expr: SRCFLAE,
      stack: List[Env] = List(Map()),
      store: Store[Val] = new NoGCStore[Val](100)
  ): (Val, Store[Val]) = expr match
    case Num(n) => (NumV(n), store)
    case Add(lhs, rhs) =>
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match
        case (NumV(n1), _) =>
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match
            case (NumV(n2), _) => (NumV(n1 + n2), s2)
            case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))
        case _ => sys.error("can only add numbers, but got: '%s' as left hand side".format(lhsv))
    case Mult(lhs, rhs) =>
      val (lhsv, s1) = interp(lhs, stack, store)
      (lhsv, s1) match
        case (NumV(n1), _) =>
          val (rhsv, s2) = interp(rhs, stack, s1)
          (rhsv, s2) match
            case (NumV(n2), _) => (NumV(n1 * n2), s2)
            case _ => sys.error("can only add numbers, but got: %s and %s".format(lhsv, rhsv))
        case _ => sys.error("can only add numbers, but got: '%s' as left hand side".format(lhsv))
    case Let(boundId, namedExpr, boundBody) =>
      val (namedVal, s1) = interp(namedExpr, stack, store)
      val (newLoc, s2) = s1.malloc(stack, namedVal)
      interp(boundBody, stack.head + (boundId -> newLoc) :: stack.tail, s2)
    case Id(name)       => (store.lookup(stack.head(name)), store)
    case Fun(arg, body) => (Closure(arg, body, stack.head), store)
    case If0(testExpr, thenExpr, elseExpr) =>
      val (testV, s1) = interp(testExpr, stack, store)
      testV match
        case NumV(n) =>
          if n == 0 then interp(thenExpr, stack, s1)
          else interp(elseExpr, stack, s1)
        case _ => sys.error("can only test numbers, but got: " + testV)
    /** In our stateful language, we do not require mutation from the host language to implement
      * cyclic environments.
      */
    case Rec(boundId, namedExpr, boundBody) =>
      val (newLoc, s2) = store.malloc(stack, NumV(0))
      val extStack = stack.head + (boundId -> newLoc) :: stack.tail
      val (namedVal, bodyStore) = interp(namedExpr, extStack, store)
      interp(boundBody, extStack, bodyStore.update(newLoc, namedVal))
    case App(funExpr, argExpr) =>
      val (funV, funStore) = interp(funExpr, stack, store)
      val (argV, argStore) = interp(argExpr, stack, funStore)
      funV match
        case Closure(fParam, fBody, fEnv) =>
          val (newLoc, resStore) = argStore.malloc(stack, argV)
          interp(fBody, fEnv + (fParam -> newLoc) :: stack, resStore)
        case _ => sys.error("can only apply functions, but got: " + funV)
    case Seqn(e1, e2) =>
      val (v1, s1) = interp(e1, stack, store)
      interp(e2, stack, s1)
    case NewBox(boxExpr) =>
      val (boxV, boxStore) = interp(boxExpr, stack, store)
      val (newLoc, resStore) = boxStore.malloc(stack, boxV)
      (Box(newLoc), resStore)
    case SetBox(boxExpr, valueExpr) =>
      val (boxV, s1) = interp(boxExpr, stack, store)
      val (value, s2) = interp(valueExpr, stack, s1)
      boxV match
        case Box(loc) => (value, s2.update(loc, value))
        case _        => sys.error("can only set to boxes, but got: " + boxV)
    case OpenBox(boxExpr) =>
      val (boxV, s1) = interp(boxExpr, stack, store)
      boxV match
        case Box(loc) => (s1.lookup(loc), s1)
        case _        => sys.error("can only open boxes, but got: " + boxV)
    case SetId(id, valExpr) =>
      val (value, s1) = interp(valExpr, stack, store)
      (value, s1.update(stack.head(id), value))

  given Conversion[String, SRCFLAE] with
    def apply(s: String): SRCFLAE = Id(s)
  given Conversion[Int, SRCFLAE] with
    def apply(n: Int): SRCFLAE = Num(n)

  def whatDoesThisDo(n: Int): SRCFLAE =
    var v: SRCFLAE = Num(17)
    for (i <- 1 to n)
      v = Seqn(NewBox(i), v)
    v

  @main def runSRCFLAEInterpreter(iterations: Int, storeSize: Int): Unit =
    val store = new NoGCStore[Val](storeSize)
    interp(whatDoesThisDo(iterations), List(Map()), store)
    println(s"all ok, final store size is ${store.nextFreeAddr - store.freed.size}")
