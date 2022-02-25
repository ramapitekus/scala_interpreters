// This file contains an interpreter for SCFLAE with simple mark&sweep garbage collection

/*
 * Based on the lecture notes for the "Programming Languages and Types"
 * course by Klaus Ostermann at the University of Marburg.
 */
package memory_management

object SRCFLAEGCInterpreter:
  /* This model of garbage collection does not illustrate the difficulty
   * of memory management. In most languages, the size of the allocated
   * memory regions on the heap vary, and hence one needs an algorithm
   * to find a free and large-enough spot on the heap. There are various
   * algorithms and heuristics (best-fit, worst-fit, first-fit, ...) for
   * that purpose.
   *
   * There are also various alternative gc designs. Mark-and-sweep is a
   * non-moving algorithm, where reachable heap objects are never moved.
   * In contrast to that, copying gc algorithms move the reachable
   * objects to a different portion of the heap. One of the oldest
   * algorithms is the semi-space garbage collector, in particular with
   * the implementation purpose.

     http://www.cs.umd.edu/class/fall2002/cmsc631/cheney/cheney.html

   * Topic for class discussion: What are the pros and cons of moving
   * vs. non-moving gc?
   *
   * It can be shown empirically that most unreachable objects become
   * unreachable while they are still young. Generational gc algorithms
   * take this empirical fact into account and divide the objects into
   * generations, whereby the (small) youngest generation of objects is
   * garbage-collected more frequently.
   *
   * A typical problem of the simple gc algorithms we discussed is the
   * stop-the-world phenomenon: All execution has to be stopped during a
   * gc cycle. This issue is addressed by incremental or concurrent
   * garbage collectors. Incremental garbage collectors typically reduce
   * the total throughput but increase responsiveness and real-time
   * behavior.
   *
   * A completely different approach to memory management is _reference
   * counting_. In reference counting, each object on the heap (in our
   * case, each box) maintains a counter which says how many pointers
   * currently point to that object. The counter is adjusted whenever a
   * pointer variable is assigned to this object (incremented), or from
   * this object to another object (decremented). When the counter is 0,
   * the object can be reclaimed.
   *
   * The obvious disadvantage of reference counting is that it cannot
   * detect cycles on the heap. Hence reference counting algorithm must
   * be augmented with some means to detect cycles.
   *
   * Topic for class discussion: What are the pros and cons of reference
   * counting vs. tracing garbage collectors such as mark-and-sweep or
   * semi-space?
   */
  class MarkAndSweepStore(maxSize: Int) extends Store[Val]:
    val memory = new Array[Val](maxSize)
    var free: Int = maxSize
    var nextFreeAddr: Int = 0

    // Only for testing: check memory contents
    def matches(mem: Map[Location, Val]): Boolean =
      val contentsMatch = mem.forall { case (loc, v) =>
        memory.indices.contains(loc) && memory(loc) == v
      }
      val restEmpty = memory.indices.filter(!mem.contains(_)).forall(memory(_) == null)
      contentsMatch && restEmpty

    def malloc(stack: List[Env], v: Val): (Location, Store[Val]) =

      // Run the GC only if there is no free memory
      if (free <= 0) v match
        case Box(l) => gc(Map("a0" -> l) :: stack)
        case _      => gc(stack)
      if (free <= 0) sys.error("out of memory")

      /* Here we find the next available location in memory via a while-
       * loop. In order to avoid maintaining a list of available spaces,
       *  let us assume that all boxes in SRCFWAE contain data
       *  (in constrast to null values).
       *
       * If we ensure the invariant that the variable `free` has always
       * the number of free memory space, then the following loop will
       * always halt. The nontermination situation will generate an out-
       * of-memory error and the program will abort.
       */

      while memory(nextFreeAddr) != null do
        nextFreeAddr += 1
        if (nextFreeAddr == maxSize) nextFreeAddr = 0

      free -= 1
      update(nextFreeAddr, v)
      (nextFreeAddr, this)

    def update(index: Location, v: Val): Store[Val] =
      memory.update(index, v)
      this

    def lookup(index: Int) = memory(index)

    def free(index: Int) =
      free += 1
      memory(index) = null
      this

    def allAddrInVal(v: Val): Set[Int] = v match
      case Box(a)                => Set(a)
      case NumV(_)               => Set()
      case Closure(f, body, env) => env.values.toSet

    def mark(seed: Set[Int]): Unit =
      for i <- seed do memory(i).marked = true

      // Find non-marked cells pointed by cells pointed by seed
      val allAddresses = seed flatMap (i => allAddrInVal(memory(i)))
      val newAddresses = allAddresses filter (i => !memory(i).marked)

      // Recursion on the next level of pointers
      if newAddresses.nonEmpty then mark(newAddresses)
    /*
     * What graph algorithm underlies the mark step as implemented here?
     * What potential problem it could cause in a "real" interpreter?
     */

    def sweep(): Unit =
      for i <- memory.indices do
        val v = memory(i)
        if v == null then ()
        /* No work needed on an empty memory cell */
        else if v.marked then
          /* Reset `marked` flag for the next gc */
          v.marked = false
        else
          free += 1
          memory(i) = null

    def gc(stack: List[Env]): Unit =
      println("\nSTARTING GC\nSTACK = " + stack + "\nSTORE = " + memory)

      val seed: Set[Int] = stack.map(env => env.values.toSet).fold(Set.empty)(_ union _)
      mark(seed)
      sweep()

      println(
        "GC COMPLETE\nSTORE = " + memory +
          "\nNUMBER OF FREE SLOTS = " + free
      )

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

  /* We equip our values with a mutable flag that is useful for
   * mark-and-sweep garbage collection. In real systems it is
   * implemented as a bit flag, or, if the so-called "tri-color
   * algorithm" is used, with two bit flags.
   */
  sealed abstract class Val(var marked: Boolean = false)
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
      store: Store[Val] = new MarkAndSweepStore(100)
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
          if (n == 0) interp(thenExpr, stack, s1)
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

  @main def runSRCFLAEGCInterpreter(iterations: Int, storeSize: Int) =
    val store = new MarkAndSweepStore(storeSize)
    val (v, s) = interp(OpenBox(NewBox(NewBox(1))), List(Map()), store)
    println(v)
    println(s)
    println(s"all ok, final store size is ${storeSize - store.free}")
