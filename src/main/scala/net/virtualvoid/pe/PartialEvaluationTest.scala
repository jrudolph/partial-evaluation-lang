package net.virtualvoid.pe

object PELang {
  sealed trait Expr
  sealed trait Literal extends Expr

  final case class ConstantString(str: String) extends Literal
  final case class ConstantInt(int: Int) extends Literal
  final case object Nil extends Literal
  final case class Lambda(body: Expr => Expr) extends Literal

  final case class Apply(f: Expr, arg: Expr) extends Expr
  final case class Plus(lhs: Expr, rhs: Expr) extends Expr
  final case class Cons(a1: Expr, a2: Expr) extends Expr
  final case class Car(arg: Expr) extends Expr
  final case class Cdr(arg: Expr) extends Expr
  final case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  final case class StringEquals(e1: Expr, e2: Expr) extends Expr

  //final case class MatchStr(lhs: Expr, matches: Seq[(String, Expr)], other: Expr) extends Expr

  object DSL {
    implicit class ExprOps(val e1: Expr) extends AnyVal {
      def +(e2: Expr): Expr = Plus(e1, e2)
      def apply(a1: Expr): Expr = Apply(e1, a1)
      def apply(a1: Expr, a2: Expr): Expr = e1(a1)(a2)

      def ->(e2: Expr): Expr = Cons(e1, e2)

      def strEquals(e2: Expr): Expr = StringEquals(e1, e2)
    }
    implicit def strAsExpr(str: String): Expr = ConstantString(str)
    implicit def intAsExpr(int: Int): Expr = ConstantInt(int)

    def expr(e: Expr): Expr = e
    def nil: Expr = Nil

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

  final case class Binding(name: String) extends Expr

  def interpret(e: Expr): Expr = {
    val result =
      e match {
        case l: Literal => l

        case Plus(ie1, ie2) =>
          val ConstantInt(i1) = interpret(ie1)
          val ConstantInt(i2) = interpret(ie2)

          ConstantInt(i1 + i2)

        case Apply(f, arg) =>
          val Lambda(fBody) = interpret(f)
          val argE = interpret(arg)
          val finalF = fBody(argE)
          interpret(finalF)

        case Cons(e1, e2) =>
          Cons(interpret(e1), interpret(e2))

        case Car(e) =>
          val Cons(a, _) = interpret(e)
          a
        case Cdr(e) =>
          val Cons(_, b) = interpret(e)
          b

        case StringEquals(e1, e2) =>
          val ConstantString(s1) = interpret(e1)
          val ConstantString(s2) = interpret(e2)
          ConstantInt(if (s1 == s2) 1 else 0)

        case IfThenElse(condE, thenExpr, elseExpr) =>
          val ConstantInt(cond) = interpret(condE)
          cond match {
            case 0 => interpret(elseExpr)
            case 1 => interpret(thenExpr)
          }

        /*case MatchStr(lhs, matches, other) =>
          val ConstantString(str) = interpret(lhs)
          val rhs = matches.find(_._1 == str).map(_._2).getOrElse(other)
          interpret(rhs)*/

        case x => throw new IllegalStateException(s"Cannot interpret $x")
      }
    println(s"Interpreting ${show(e)} to ${show(result)}")
    result
  }

  def show(e: Expr): String = {
    var nameCounter: Int = 0

    def doShow(e: Expr): String = e match {
      case Plus(e1, e2)         => s"(${doShow(e1)} + ${doShow(e2)})"
      case ConstantInt(i)       => i.toString
      case ConstantString(s)    => "\"" + s + "\""
      case Apply(l, arg)        => s"(${doShow(l)})(${doShow(arg)})"
      case Nil                  => "nil"
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

      case Lambda(body) =>
        nameCounter += 1
        val binding = Binding(s"x$nameCounter")
        val f = body(binding)
        s"${binding.name} => ${doShow(f)}"
      case Binding(n) => n
    }
    doShow(e)
  }
}

object PartialEvaluationTest extends App {
  import PELang._
  import PELang.DSL._
  val plus5 = lambda(x => x + 5)
  val plus = lambda2(_ + _)
  //println(PELang.interpret((5: Expr) + 38))
  //println(PELang.interpret(plus5(38)))
  //println(PELang.interpret(plus(5, 38)))

  //println(PELang.interpret(car((5: Expr) -> 32)))

  // Representation of expr in PELang
  // ("str", "string") -> ConstantString
  // ("int", 5) -> ConstantInt
  // ("nil", nil) -> Nil
  // ("lambda", x => body(x)) -> Lambda
  // ("apply", (f, arg)) -> Apply
  // ("plus", (e1, e2)) -> Plus
  // ("cons", (e1, e2)) -> Cons
  // ("car", e) -> Car
  // ("cdr", e) -> Cdr
  // ("strequals", (e1, e2)) -> StringEquals
  // ("ifthenelse", (cond, (thenExpr, elseExpr))) -> IfThenElse

  // classical strict fixed-point combinator as shown here: https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator
  val fix: Expr = lambda(f => lambda(x => f(lambda(v => x(x)(v))))(lambda(x => f(lambda(v => x(x)(v))))))
  val interpreter = fix {
    lambda { iRec =>
      lambda { e =>
        matchStr(
          car(e),
          Seq(
            "str" -> cdr(e),
            "int" -> cdr(e),
            "nil" -> nil,
            "lambda" -> cdr(e),
            "plus" -> (iRec(car(cdr(e))) + iRec(cdr(cdr(e)))),
            "cons" -> (iRec(car(cdr(e))) -> iRec(cdr(cdr(e)))),
            "apply" -> iRec(iRec(car(cdr(e))).apply(iRec(cdr(cdr(e))))),
            "car" -> car(iRec(cdr(e))),
            "cdr" -> cdr(iRec(cdr(e))),
            "strequals" -> (iRec(car(cdr(e))) strEquals iRec(cdr(cdr(e)))),
            "ifthenelse" -> ifThenElse(iRec(car(cdr(e))), iRec(car(cdr(cdr(e)))), iRec(cdr(cdr(cdr(e)))))
          ), "failed")
      }
    }
  }

  def reify(e: Expr): Expr = e match {
    case c: ConstantString => expr("str") -> c
    case i: ConstantInt    => expr("int") -> i
    case Plus(e1, e2)      => expr("plus") -> (reify(e1) -> reify(e2))
    case Cons(e1, e2)      => expr("cons") -> (reify(e1) -> reify(e2))
    case Nil               => expr("nil") -> nil
    case Apply(f, arg)     => expr("apply") -> (reify(f) -> reify(arg))
    case Lambda(f)         => expr("lambda") -> lambda(x => reify(f(x)))
    case Binding(x)        => expr("binding") -> x // only needed for show
  }

  //def partiallyEvaluate(f: Expr, )

  val reifiedAppliedPlus5 = expr("apply") -> ((expr("lambda") -> lambda(x => expr("plus") -> (x -> (expr("int") -> 5)))) -> (expr("int") -> 12))
  println(show(reifiedAppliedPlus5))
  println(show(reify(plus5(12))))
  val triplus = lambda3((x, y, z) => x + y + z)
  println(show(interpret(lambda(x => triplus(x + 12, 38)))))
  println(show(PELang.interpret(interpreter(reify(plus5)))))
  //println(show(PELang.interpret(interpreter(reify(plus5)))))
}
