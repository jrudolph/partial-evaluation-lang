package net.virtualvoid.pe

import java.util.concurrent.atomic.AtomicLong

object PELang {

  sealed trait Value extends Product {
    def asInt: Int = asInstanceOf[IntValue].int
    def asString: String = asInstanceOf[StringValue].str
    def asCons: ConsValue = asInstanceOf[ConsValue]
    def isNil: Boolean = this eq Nil
    def asLambda: Lambda = this match {
      case l: Lambda => l
      case x         => throw new IllegalArgumentException(s"Expected lambda but found [$x] (${x.productPrefix})")
    }
  }
  final case class StringValue(str: String) extends Value
  final case class IntValue(int: Int) extends Value
  final case object Nil extends Value
  final case class Lambda(binding: Binding, body: Expr, extraBindings: Map[Binding, Value] = Map.empty) extends Value
  final case class ConsValue(a1: Value, a2: Value) extends Value

  sealed trait Expr extends Product
  final case class Literal(value: Value) extends Expr
  final case class Apply(f: Expr, arg: Expr) extends Expr
  final case class Plus(lhs: Expr, rhs: Expr) extends Expr
  final case class Cons(a1: Expr, a2: Expr) extends Expr
  final case class Car(arg: Expr) extends Expr
  final case class Cdr(arg: Expr) extends Expr
  final case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  final case class StringEquals(e1: Expr, e2: Expr) extends Expr
  final case class Binding(name: String) extends Expr

  //final case class MatchStr(lhs: Expr, matches: Seq[(String, Expr)], other: Expr) extends Expr

  object DSL {
    implicit class ExprOps(val e1: Expr) extends AnyVal {
      def +(e2: Expr): Expr = Plus(e1, e2)
      def apply(a1: Expr): Expr = Apply(e1, a1)
      def apply(a1: Expr, a2: Expr): Expr = e1(a1)(a2)

      def ->(e2: Expr): Expr = Cons(e1, e2)

      def strEquals(e2: Expr): Expr = StringEquals(e1, e2)
    }
    implicit class ValueOps(val v1: Value) extends AnyVal {
      def ->(v2: Value): Value = ConsValue(v1, v2)
    }
    implicit def valueAsLiteral(value: Value): Expr = Literal(value)
    implicit def strAsExpr(str: String): Expr = StringValue(str)
    implicit def intAsExpr(int: Int): Expr = IntValue(int)

    implicit def strAsValue(str: String): Value = StringValue(str)
    implicit def Value(int: Int): Value = IntValue(int)

    def value(v: Value): Value = v
    def expr(e: Expr): Expr = e
    def nil: Value = Nil

    private val varCounter = new AtomicLong()
    private def binding(): Binding = Binding(s"x_${varCounter.incrementAndGet()}")
    def Lambda(body: Expr => Expr): Expr = {
      val b = binding()
      new Lambda(b, body(b))
    }

    def lambda(body: Expr => Expr): Expr = Lambda(body)
    def lambda2(body: (Expr, Expr) => Expr): Expr = Lambda(a1 => Lambda(a2 => body(a1, a2)))
    def lambda3(body: (Expr, Expr, Expr) => Expr): Expr = Lambda(a1 => Lambda(a2 => Lambda(a3 => body(a1, a2, a3))))

    def car(a: Expr): Expr = Car(a)
    def cdr(a: Expr): Expr = Cdr(a)

    def ifThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr): Expr = IfThenElse(cond, thenExpr, elseExpr)
    def matchStr(lhs: Expr, matches: Seq[(String, Expr)], other: Expr): Expr =
      matches.foldRight(other) { (x, elseExpr) =>
        val (candidate, thenExpr) = x
        IfThenElse(StringEquals(lhs, candidate), thenExpr, elseExpr)
      }
  }

  def interpret(e: Expr, bindings: Map[Binding, Value] = Map.empty, trace: Boolean = false): Value = {
    def rec(e: Expr, bindings: Map[Binding, Value] = bindings, trace: Boolean = trace): Value = {
      if (trace) println(s"Interpreting ${e.productPrefix} ${show(e)} with bindings [${bindings.map(b => s"${b._1.name} -> ${show(b._2)}").mkString(", ")}]")

      val result: Value =
        e match {
          case Literal(Lambda(b, body, extra)) => Lambda(b, body, extra ++ bindings) // FIXME: replace free variables directly
          case Literal(value)                  => value

          case Plus(ie1, ie2)                  => IntValue(rec(ie1).asInt + rec(ie2).asInt)

          case Apply(f, arg) =>
            val Lambda(binding, body, extra) = rec(f).asLambda
            val argE = rec(arg)
            interpret(body, (bindings ++ extra) + (binding -> argE))

          case Cons(e1, e2)         => ConsValue(rec(e1), rec(e2))
          case Car(e)               => rec(e).asCons.a1
          case Cdr(e)               => rec(e).asCons.a2

          case StringEquals(e1, e2) => IntValue(if (rec(e1).asString == rec(e2).asString) 1 else 0)

          case IfThenElse(condE, thenExpr, elseExpr) =>
            rec(condE).asInt match {
              case 0 => rec(elseExpr)
              case 1 => rec(thenExpr)
            }

          case b: Binding => bindings(b)

          /*case MatchStr(lhs, matches, other) =>
          val ConstantString(str) = interpret(lhs)
          val rhs = matches.find(_._1 == str).map(_._2).getOrElse(other)
          interpret(rhs)*/

          case x          => throw new IllegalStateException(s"Cannot interpret $x")
        }
      if (trace) println(s"Interpreting ${show(e)} with bindings [${bindings.map(b => s"${b._1.name} -> ${show(b._2)}").mkString(", ")}] to ${show(result)}")
      result
    }
    rec(e)
  }

  def show(v: Value): String = v match {
    case IntValue(i)                  => i.toString
    case StringValue(s)               => "\"" + s + "\""
    case ConsValue(e1, e2)            => s"(${show(e1)} . ${show(e2)})"
    case Nil                          => "nil"
    case Lambda(binding, body, extra) => s"${binding.name} => ${show(body)} [${extra.map(b => s"${b._1.name} -> ${show(b._2)}").mkString(", ")}]"
  }
  def show(e: Expr): String = {
    def doShow(e: Expr): String = e match {
      case Plus(e1, e2)         => s"(${doShow(e1)} + ${doShow(e2)})"

      case Apply(l, arg)        => s"(${doShow(l)})(${doShow(arg)})"

      case Cons(e1, e2)         => s"(${doShow(e1)} . ${doShow(e2)})"
      case Car(a)               => s"car(${doShow(a)})"
      case Cdr(a)               => s"cdr(${doShow(a)})"

      case StringEquals(e1, e2) => s"${doShow(e1)} == ${doShow(e2)}"
      case IfThenElse(cond, thenExpr, elseExpr) =>
        s"""if (${doShow(cond)})
           |then ${doShow(thenExpr)}
           |else ${doShow(elseExpr)}""".stripMargin
      /*case MatchStr(lhs, matches, other) =>
        s"${doShow(lhs)} match {\n" + matches.map { case (str, e) => s""""$str" => ${doShow(e)}""" }.mkString("\n") + "}\n"*/

      case Binding(n) => n
      case Literal(v) => show(v)
    }
    doShow(e)
  }
}
object MetaPE {
  import PELang._
  import PELang.DSL._

  // Representation of expr in PELang
  // ("str", "string") -> ConstantString
  // ("int", 5) -> ConstantInt
  // ("nil", nil) -> Nil
  // ("lambda", (<name of parameter>, body)) -> Lambda
  // ("apply", (f, arg)) -> Apply
  // ("plus", (e1, e2)) -> Plus
  // ("cons", (e1, e2)) -> Cons
  // ("car", e) -> Car
  // ("cdr", e) -> Cdr
  // ("strequals", (e1, e2)) -> StringEquals
  // ("ifthenelse", (cond, (thenExpr, elseExpr))) -> IfThenElse
  // ("binding", <name>)

  // classical strict fixed-point combinator as shown here: https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator
  val fix: Expr = lambda(f => lambda(x => f(lambda(v => x(x)(v))))(lambda(x => f(lambda(v => x(x)(v))))))
  // an interpreter written in PE that can interpret metaPE programs, i.e. evaluates a metaPE expression to a constant
  val interpreter = fix {
    lambda { iRec =>
      lambda { e =>
        matchStr(
          car(e),
          Seq(
            "str" -> cdr(e),
            "int" -> cdr(e),
            "nil" -> nil,
            "consV" -> (iRec(car(cdr(e))) -> iRec(cdr(cdr(e)))),
            "lambda" -> cdr(e),
            "plus" -> (iRec(car(cdr(e))) + iRec(cdr(cdr(e)))),
            "consF" -> (iRec(car(cdr(e))) -> iRec(cdr(cdr(e)))),
            "apply" -> iRec(car(cdr(e))).apply(iRec(cdr(cdr(e)))),
            "car" -> car(iRec(cdr(e))),
            "cdr" -> cdr(iRec(cdr(e))),
            "strequals" -> (iRec(car(cdr(e))) strEquals iRec(cdr(cdr(e)))),
            "ifthenelse" -> ifThenElse(iRec(car(cdr(e))), iRec(car(cdr(cdr(e)))), iRec(cdr(cdr(cdr(e)))))
          ), expr("failed to interpret") -> e)
      }
    }
  }

  def reify(v: Value): Value = v match {
    case c: StringValue    => value("str") -> c
    case i: IntValue       => value("int") -> i
    case Nil               => value("nil") -> nil
    case l: Lambda         => value("lambda") -> l // FIXME: it's a bit lazy to represent lambdas non-explicitly, will it bite us later, though?
    case ConsValue(a1, a2) => value("consV") -> (reify(a1) -> reify(a2))
  }
  def reify(e: Expr): Value = e match {
    case Plus(e1, e2)         => value("plus") -> (reify(e1) -> reify(e2))
    case Cons(e1, e2)         => value("consF") -> (reify(e1) -> reify(e2))
    case Car(e)               => value("car") -> reify(e)
    case Cdr(e)               => value("cdr") -> reify(e)
    case Apply(f, arg)        => value("apply") -> (reify(f) -> reify(arg))
    case StringEquals(e1, e2) => value("strequals") -> (reify(e1) -> reify(e2))
    case IfThenElse(c, t, e)  => value("ifthenelse") -> (reify(c) -> (reify(t) -> reify(e)))
    case Literal(v)           => reify(v)
    case Binding(x)           => value("binding") -> x
  }

  def pe(e: Expr): Expr = {
    def pe(e: Expr, bindings: Map[Binding, Value] = Map.empty): Expr = {
      val result: Expr =
        e match {
          case Literal(Lambda(b, body, extra)) => Literal(PELang.Lambda(b, body, bindings ++ extra))
          case Literal(l)                      => l
          case b: Binding                      => bindings.get(b).fold[Expr](b)(Literal)
          case Plus(e1, e2) =>
            (pe(e1, bindings), pe(e2, bindings)) match {
              case (Literal(IntValue(i1)), Literal(IntValue(i2))) => expr(i1 + i2)
              // error on mismatch types
              // add optimizations by int rules (like `x + 0` etc)
              case (e1, e2)                                       => Plus(e1, e2)
            }
          case Cons(e1, e2) =>
            (pe(e1, bindings), pe(e2, bindings)) match {
              case (Literal(v1), Literal(v2)) => Literal(ConsValue(v1, v2))
              case (e1, e2)                   => Cons(e1, e2)
            }
          case Car(e) =>
            pe(e, bindings) match {
              case Literal(ConsValue(a1, _)) => Literal(a1)
              // error on mismatch types
              case x                         => Car(x)
            }
          case Cdr(e) =>
            pe(e, bindings) match {
              case Literal(ConsValue(_, a2)) => Literal(a2)
              // error on mismatch types
              case x                         => Cdr(x)
            }
          case Apply(f, arg) =>
            (pe(f, bindings), pe(arg, bindings)) match {
              case (Literal(Lambda(b, body, extra)), Literal(value)) => pe(body, (bindings ++ extra) + ((b, value)))
              // error on mismatch types
              case (f, arg) => Apply(f, arg)
            }
          case StringEquals(e1, e2) =>
            (pe(e1, bindings), pe(e2, bindings)) match {
              case (Literal(StringValue(str1)), Literal(StringValue(str2))) => if (str1 == str2) Literal(1) else Literal(0)
              // error on mismatch types
              case (e1, e2) => StringEquals(e1, e2)
            }
          case IfThenElse(cond, thenExpr, elseExpr) =>
            pe(cond, bindings) match {
              case Literal(IntValue(1)) => pe(thenExpr, bindings)
              case Literal(IntValue(0)) => pe(elseExpr, bindings)
              case c                    => IfThenElse(c, pe(thenExpr, bindings), pe(elseExpr, bindings))
            }
        }

      //println(s"PE ${show(e)} with bindings [${bindings.map(b => s"${b._1.name} -> ${show(b._2)}").mkString(", ")}] to ${show(result)}")
      result
    }

    pe(e)
  }
}