package net.virtualvoid.pe

object PartialEvaluationTest extends App {
  import PELang.DSL._
  import PELang._
  val plus5 = lambda(x => x + 5)
  val plus = lambda2(_ + _)
  //println(PELang.interpret((5: Expr) + 38))
  //println(PELang.interpret(plus5(38)))
  //println(PELang.interpret(plus(5, 38)))

  //println(PELang.interpret(car((5: Expr) -> 32)))

  //val metaPe = lambda

  //def partiallyEvaluate(f: Expr, )

  val reifiedAppliedPlus5 = expr("apply") -> ((expr("lambda") -> lambda(x => expr("plus") -> (x -> (expr("int") -> 5)))) -> (expr("int") -> 12))
  println(show(PELang.interpret(expr(12) + 5)))
  //println(show(reify(plus5(12))))
  //println(show(PELang.interpret(interpreter(reify(expr(12) + 5)))))
  //println(show(reifiedAppliedPlus5))
  //println(show(PELang.interpret(interpreter(reifiedAppliedPlus5))))
  //println(show(reify(plus5(12))))
  //val triplus = lambda3((x, y, z) => x + y + z)
  //println(show(interpret(lambda(x => triplus(x + 12, 38)))))
  //println(show(PELang.interpret(interpreter(reify(plus5)))))
  //println(show(PELang.interpret(interpreter(reify(plus5)))))
}
