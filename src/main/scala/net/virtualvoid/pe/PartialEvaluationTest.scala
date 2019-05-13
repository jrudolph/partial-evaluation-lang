package net.virtualvoid.pe

object PELang {
  sealed trait Expr

  final case class Lambda(body: Expr => Expr) extends Expr
  final case class Apply(f: Expr, arg: Expr) extends Expr
  final case class ConstantString(str: String) extends Expr
  final case class ConstantInt(int: Int) extends Expr
  final case class Plus(lhs: Expr, rhs: Expr) extends Expr

  object DSL {
    implicit class ExprOps(val e: Expr) extends AnyVal {
      def +(other: Expr): Expr = Plus(e, other)
      def apply(other: Expr): Expr = Apply(e, other)
    }
    implicit def strAsExpr(str: String): Expr = ConstantString(str)
    implicit def intAsExpr(int: Int): Expr = ConstantInt(int)

    def lambda(body: Expr => Expr): Expr = Lambda(body)
  }

  def interpret(e: Expr): Expr = {
    final case class Binding(name: String) extends Expr

    def doInterpret(e: Expr, bindings: Map[String, Expr], nameCounter: Int): Expr = e match {
      case Plus(ie1, ie2) =>
        val ConstantInt(i1) = doInterpret(ie1, bindings, nameCounter)
        val ConstantInt(i2) = doInterpret(ie2, bindings, nameCounter)

        ConstantInt(i1 + i2)
      case c: ConstantInt    => c
      case c: ConstantString => c
      case l: Lambda         => l
      case Apply(f, arg) =>
        val Lambda(fBody) = doInterpret(f, bindings, nameCounter)
        val binding = Binding(s"x$nameCounter")
        val finalF = fBody(binding)
        val argE = doInterpret(arg, bindings, nameCounter)
        doInterpret(finalF, bindings + (binding.name -> argE), nameCounter + 1)
      case Binding(name) => bindings(name)
      case x             => throw new IllegalStateException(s"Cannot interpret $x with bindings $bindings")
    }

    doInterpret(e, Map.empty, 0)
  }
}

object PartialEvaluationTest extends App {
  import PELang.Expr
  import PELang.DSL._
  val plus5 = lambda(x => x + 5)
  println(PELang.interpret((5: Expr) + 38))
  println(PELang.interpret(plus5(38)))
}
