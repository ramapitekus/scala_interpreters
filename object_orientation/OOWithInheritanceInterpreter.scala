package object_orientation

object OOWithInheritanceInterpreter:

  sealed abstract class Expr
  case class New(className: String, fields: List[Expr]) extends Expr
  case class FAcc(objExpr: Expr, fieldName: String) extends Expr
  case class Invoke(objExpr: Expr, methodName: String, args: List[Expr]) extends Expr
  case class Id(id: String) extends Expr

  case class Class(superClass: String, fields: List[String], methods: Map[String, Method])

  case class Method(params: List[String], body: Expr)

  sealed abstract class Value
  case class Object(className: String, fields: List[Value]) extends Value

  type Env = Map[String, Value]

  // Returns all fields in the path from the root to className in the inheritance tree
  def lookupField(
      fieldName: String,
      className: String,
      fieldVals: List[Value],
      classes: Map[String, Class]
  ): Value = className match
    case "Object" => sys.error("Unknown field %s".format(fieldName))
    case _ =>
      val clazz = classes.getOrElse(className, sys.error("Unknown class %s".format(className)))
      val index = clazz.fields.indexOf(fieldName)
      if index >= 0 then fieldVals(index)
      else lookupField(fieldName, clazz.superClass, fieldVals.drop(clazz.fields.size), classes)

  // Returns the first method found in the path from className to the root in the inheritance tree or None
  def lookupMethod(
      methodName: String,
      className: String,
      classes: Map[String, Class]
  ): Option[Method] = className match
    case "Object" => None
    case _ =>
      val clazz = classes.getOrElse(className, sys.error("Unknown class %s".format(className)))
      if (clazz.methods.contains(methodName)) Some(clazz.methods(methodName))
      else lookupMethod(methodName, clazz.superClass, classes)

  def interp(e: Expr, env: Env, classes: Map[String, Class]): Value = e match
    case New(className, args) =>
      if (!classes.contains(className))
        sys.error("Can not initialize unknown class %s".format(className))
      Object(className, args map { interp(_, env, classes) })
    case FAcc(objExpr, fieldName) =>
      val maybeObj = interp(objExpr, env, classes)
      maybeObj match
        case Object(className, fields) => lookupField(fieldName, className, fields, classes)
        case _                         => sys.error("Expected object, but got %s".format(maybeObj))
    case Invoke(objExpr, methodName, args) =>
      val maybeObj = interp(objExpr, env, classes)
      maybeObj match
        case Object(className, fieldVals) =>
          val method = lookupMethod(methodName, className, classes) getOrElse
            sys.error("Unknown method %s for class %s".format(methodName, className))
          val argVals = args map { interp(_, env, classes) }
          val argBindings = method.params zip argVals
          val thisBinding = "this" -> maybeObj
          val newEnv = Map() ++ argBindings + thisBinding
          interp(method.body, newEnv, classes)
        case _ => sys.error("Expected object, but got %s".format(maybeObj))
    case Id(id) => env(id)
