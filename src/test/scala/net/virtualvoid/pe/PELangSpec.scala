package net.virtualvoid.pe

import net.virtualvoid.pe.PELang._
import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

object PELangGen {
  /*
  final case class StringValue(str: String) extends Value
  final case class IntValue(int: Int) extends Value
  final case object Nil extends Value
  final case class Lambda(binding: Binding, body: Expr) extends Value
  final case class ConsValue(a1: Value, a2: Value) extends Value

  sealed trait Expr
  final case class Literal(value: Value) extends Expr
  final case class Apply(f: Expr, arg: Expr) extends Expr
  final case class Plus(lhs: Expr, rhs: Expr) extends Expr
  final case class Cons(a1: Expr, a2: Expr) extends Expr
  final case class Car(arg: Expr) extends Expr
  final case class Cdr(arg: Expr) extends Expr
  final case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
  final case class StringEquals(e1: Expr, e2: Expr) extends Expr
  final case class Binding(name: String) extends Expr
   */
  val stringValGen = Gen.alphaStr.map(StringValue)
  val intValGen = arbitrary[Int].map(IntValue)
  val nilGen = const(Nil)
  lazy val consValGen: Gen[ConsValue] =
    for {
      v1 <- anyValueGen
      v2 <- anyValueGen
    } yield ConsValue(v1, v2)
  // lazy val lambdaGen -- complicated

  lazy val anyValueGen: Gen[Value] = oneOf(stringValGen, intValGen, nilGen, lzy(consValGen))

  val anyLitGen: Gen[Literal] = anyValueGen.map(Literal)

  val intLitGen: Gen[Literal] = intValGen.map(Literal)
  val consLitGen: Gen[Literal] = consValGen.map(Literal)
  val stringLitGen: Gen[Literal] = stringValGen.map(Literal)

  lazy val plusExprGen: Gen[Expr] =
    for {
      i1 <- intExprGen
      i2 <- intExprGen
    } yield Plus(i1, i2)

  lazy val intExprGen: Gen[Expr] = oneOf(intLitGen, lzy(plusExprGen), stringEqualsExprGen)
  lazy val consFExprGen: Gen[Expr] =
    for {
      a1 <- anyExprGen
      a2 <- anyExprGen
    } yield Cons(a1, a2)
  lazy val consExprGen: Gen[Expr] = oneOf(consLitGen, consFExprGen)

  lazy val carExprGen: Gen[Expr] = consExprGen.map(Car)
  lazy val cdrExprGen: Gen[Expr] = consExprGen.map(Cdr)

  lazy val stringExprGen: Gen[Expr] = stringLitGen // only way to generate strings so far
  lazy val stringEqualsExprGen: Gen[Expr] =
    for {
      str1 <- stringExprGen
      str2 <- stringExprGen
    } yield StringEquals(str1, str2)

  lazy val ifThenElseExprGen: Gen[Expr] =
    for {
      cond <- stringEqualsExprGen
      thenB <- anyExprGen
      elseB <- anyExprGen
    } yield IfThenElse(cond, thenB, elseB)

  lazy val anyExprGen: Gen[Expr] =
    oneOf(
      anyLitGen,
      plusExprGen,
      lzy(consFExprGen),
      lzy(carExprGen),
      lzy(cdrExprGen),
      stringEqualsExprGen,
      lzy(ifThenElseExprGen)
    )
}

class PELangSpec extends AnyWordSpec with ScalaCheckPropertyChecks with Matchers {
  import PELangGen._
  import PELang.interpret
  import PELang.DSL._

  "PELang" should {
    "interpret" should {
      "literals" in {
        forAll(anyLitGen) { lit =>
          interpret(lit) mustEqual lit.value
        }
      }
      "any generateable expressions" in {
        forAll(anyExprGen) { expr =>
          interpret(expr) must not equal null // just checks that it doesn't fail
        }
      }
      "car/cons" in {
        forAll(anyLitGen, anyLitGen) { (e1, e2) =>
          interpret(car(e1 -> e2)) mustEqual e1.value
        }
      }
      "cdr/cons" in {
        forAll(anyLitGen, anyLitGen) { (e1, e2) =>
          interpret(cdr(e1 -> e2)) mustEqual e2.value
        }
      }
      "if / then / else" in {}
      "stringEquals" in {}
      "apply lambda" in {
        forAll(intLitGen, intLitGen) { (i1, i2) =>
          interpret(lambda(_ + i1)(i2)).asInt mustEqual i1.value.asInt + i2.value.asInt
        }
        forAll(intExprGen, intExprGen) { (i1, i2) =>
          interpret(lambda(_ + i1)(i2)).asInt mustEqual interpret(i1).asInt + interpret(i2).asInt
        }
      }
      "support closures" in {
        forAll(intExprGen, intExprGen) { (i1, i2) =>
          interpret(lambda(x => lambda(y => x + y))(i1)(i2)).asInt mustEqual interpret(i1).asInt + interpret(i2).asInt
        }
      }
    }
  }

  "The PELang meta-interpreter" should {
    "evaluate cons literals to themselves" in {
      val lit = ConsValue(Nil, Nil)
      val reified = MetaPE.reify(lit)
      interpret(MetaPE.interpreter(reified)) mustEqual ConsValue(Nil, Nil)
    }
    "evaluate literals to themselves" in {
      forAll(anyLitGen) { lit =>
        interpret(MetaPE.interpreter(MetaPE.reify(lit))) mustEqual lit.value
      }
    }
    "work for arbitrary int expressions" in {
      forAll(intExprGen) { expr =>
        interpret(MetaPE.interpreter(MetaPE.reify(expr))) mustEqual interpret(expr)
      }
    }
  }
}
