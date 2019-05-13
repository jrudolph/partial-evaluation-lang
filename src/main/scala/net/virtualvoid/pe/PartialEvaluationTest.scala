package net.virtualvoid.pe

object PELang {
  sealed trait Expr
  sealed trait Literal extends Expr

  final case class ConstantString(str: String) extends Literal
  final case class ConstantInt(int: Int) extends Literal
  final case class Lambda(body: Expr => Expr) extends Literal

  final case class Apply(f: Expr, arg: Expr) extends Expr
  final case class Plus(lhs: Expr, rhs: Expr) extends Expr

  object DSL {
    implicit class ExprOps(val e: Expr) extends AnyVal {
      def +(other: Expr): Expr = Plus(e, other)
      def apply(a1: Expr): Expr = Apply(e, a1)
      def apply(a1: Expr, a2: Expr): Expr = e(a1)(a2)
    }
    implicit def strAsExpr(str: String): Expr = ConstantString(str)
    implicit def intAsExpr(int: Int): Expr = ConstantInt(int)

    def lambda(body: Expr => Expr): Expr = Lambda(body)
    def lambda2(body: (Expr, Expr) => Expr): Expr = Lambda(a1 => Lambda(a2 => body(a1, a2)))
    def lambda3(body: (Expr, Expr, Expr) => Expr): Expr = Lambda(a1 => Lambda(a2 => Lambda(a3 => body(a1, a2, a3))))
  }

  private final case class Binding(name: String) extends Expr

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

        case x => throw new IllegalStateException(s"Cannot interpret $x")
      }
    println(s"Interpreting ${show(e)} to ${show(result)}")
    result
  }

  def show(e: Expr): String = {
    var nameCounter: Int = 0

    def doShow(e: Expr): String = e match {
      case Plus(e1, e2)      => s"(${doShow(e1)} + ${doShow(e2)})"
      case ConstantInt(i)    => i.toString
      case ConstantString(s) => s
      case Apply(l, arg)     => s"(${doShow(l)})(${doShow(arg)})"
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
  import PELang.Expr
  import PELang.DSL._
  val plus5 = lambda(x => x + 5)
  val plus = lambda2(_ + _)
  //println(PELang.interpret((5: Expr) + 38))
  //println(PELang.interpret(plus5(38)))
  println(PELang.interpret(plus(5, 38)))

  val interpreter = lambda2 { (e, bindings) =>
    0
  }
}
