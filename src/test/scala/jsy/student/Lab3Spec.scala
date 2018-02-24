package jsy.student

import jsy.lab3.Lab3Like
import jsy.lab3.Parser.parse
import jsy.lab3.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

class Lab3Spec(lab3: Lab3Like) extends FlatSpec {
  import lab3._
//First, try some of the lab 2 stuff -- see if it still works here

  "And" should "return true only if both expressions are true" in {
    val env: Env = empty
    val t = B(true)
    val f = B(false)
    val x = S("Hello")
    assert(eval(env, Binary(And,t,t)) === t)
    assert(eval(env, Binary(And,t,f)) === f)
    assert(eval(env, Binary(And,f,t)) === f)
    assert(eval(env, Binary(And,f,f)) === f)
    assert(eval(env, Binary(And,x,t)) === t) //added this
  }
  "Plus" should "add two number values and return a number" in {
    val env: Env = empty
    val e1 = N(1)
    val e2 = N(2)
    val e3 = eval(env, Binary(Plus, e1, e2))
    assert(e3 === N(3))
  }

  it should "return non-intuitive results from differing types" in { //Added this
    val env: Env = empty
    val e1 = S("Hello")
    val e2 = B(false)
    val e3 = N(123)
    val e4 = eval(env, Binary(Plus, e1, e2))
    val e5 = eval(env, Binary(Plus, e3, e2))
    assert(e4 === S("Hellofalse"))
    assert(e5 === N(123))
  }

  "Lt" should "return true if the first expression is less than the second" in {
    val env: Env = empty
    val e1 = N(5)
    val e2 = N(7)
    val e3 = eval(env, Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  "eval/function" should "be considered values" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Var(x))
    val e2 = Function(Some(f), x, Var(x))
    assert(evaluate(e1) == e1)
    assert(evaluate(e2) == e2)
  }

  "eval/call" should "evaluate a function using big-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(3))
  }

  it should "handle recursive functions using big-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(6))
  }

  "step/call" should "evaluate a function using small-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(3))
  }

  it should "handle recursive functions using small-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(6))
  }

  "substitute" should "perform syntatic substitution respecting shadowing" in {
    val xplus1 = parse("x + 1")
    val twoplus1 = parse("2 + 1")
    assert(substitute(xplus1, N(2), "x") === twoplus1)
    val constx3 = parse("const x = 3; x")
    val shadowx = Binary(Plus, constx3, Var("x"))
    assert(substitute(shadowx, N(2), "x") === Binary(Plus, constx3, N(2)))
  }

  {
    val one = parse("1")

    "iterate" should "stop if the callback body returns None" in {
      assertResult(one) {
        iterate(one) { (_, _) => None }
      }
    }

    it should "increment the loop counter on each iteration and use e if the callback body returns Some(e)" in {
      assertResult(parse("--1")) {
        iterate(one) { (e: Expr, n: Int) =>
          if (n == 2) None else Some(Unary(Neg, e))
        }
      }
    }
  }

  /* Tests based on rules */

  {
    val xval = N(2)
    val envx = extend(empty, "x", xval)
    val varx = Var("x")

    val e1 = parse("2 - 1 - 1")
    val e1p = parse("1 - 1")
    val e2 = parse("3 - 1 - 1")
    val e2p = parse("2 - 1")
    val v1 = N(0)
    val v2 = N(1)
    val e3 = parse("'hello' + 2")

    val vidfunction = parse("function (x) { return x }")

    "EvalVar" should "perform EvalVar" in {
      assertResult(xval) {
        eval(envx, varx)
      }
    }

    "EvalNeg" should "perform EvalNeg" in {
      val np = -toNumber(v1)
      assertResult(N(np)) {
        eval(envx, Unary(Neg, e1))
      }
    }

    "EvalTypeErrorEquality1" should "perform EvalTypeErrorEquality1" in {
      intercept[DynamicTypeError] {
        eval(envx, Binary(Eq, vidfunction, e2))
      }
    }

    "DoNeg" should "perform DoNeg" in {
      val np = -toNumber(v1)
      assertResult(N(np)) {
        step(Unary(Neg, v1))
      }
    }

    "SearchUnary" should "perform SearchUnary" in {
      assertResult(Unary(Neg, e1p)) {
        step(Unary(Neg, e1))
      }
    }

    "SearchBinary" should "perform SearchBinary" in { //Added this
      assertResult(Binary(Minus, N(1), e2)) {
        step(Binary(Minus, e2p, e2))
      }
    }

    "SearchBinary" should "perform SearchBinary with non-intuitive values" in { //Added this
      assertResult(S("hello1")) {
        step(Binary(Plus, S("hello"), N(1)))
      }
      assertResult(Binary(Plus, B(true), S("hello2"))) {
        step(Binary(Plus, B(true), e3))
      }
    }

    "SearchConst" should "perform SearchConst" in { //Added this
      assertResult(Binary(Plus, N(4), N(2))){
        step(ConstDecl("x", N(4), Binary(Plus, Var("x"), N(2))))
      }
      assertResult(ConstDecl("y", N(3), Binary(Minus, N(4), Var("y")))){
        step(ConstDecl("x", N(4), ConstDecl("y", N(3), Binary(Minus, Var("x"), Var("y")))))
      }
      assertResult(Binary(Minus, N(4), N(3))){
        step(step(ConstDecl("x", N(4), ConstDecl("y", N(3), Binary(Minus, Var("x"), Var("y"))))))
      }
    }

  }
}

// An adapter class to pass in your Lab3 object.
class Lab3SpecRunner extends Lab3Spec(Lab3)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab3.
// The test expects a corresponding .ans file with the expected result.
class Lab3JsyTests extends JavascriptyTester(None, "lab3", Lab3)

class Lab3Suite extends Suites(
  new Lab3SpecRunner,
  new Lab3JsyTests
)
