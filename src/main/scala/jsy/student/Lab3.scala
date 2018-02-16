package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._
  
  /*
   * CSCI 3155: Lab 3 
   * Jesse Gonzales
   * 
   * Partner:
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */
  
  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */
  
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S(s) => try {
        val x = s.toDouble
        if (x.isWhole()) x.toInt else x
      } catch {
        case e: NumberFormatException => Double.NaN
      }
      case Function(_, _, _) => Double.NaN
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case Function(_, _, _) => true
      case S(s) => false
      case Undefined => false
      case N(n) => n match {
        case 0 => false
        case Double.NaN => false
        case _ => true
      }
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
        // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
        // of the function (from the input program).
      case Function(_, _, _) => "function"
      case N(n) => if(n.isWhole()) n.toInt.toString()  else n.toString()
      case B(b) => if (b) "true" else "false"
      case Undefined => "undefined"
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (Undefined,v2) => false
      case (v1, Undefined) => false
      case (Undefined, Undefined) => false
      case (S(_), S(_)) => bop match {
        case Lt => if (toStr(v1) < toStr(v2)) true else false
        case Le => if (toStr(v1) <= toStr(v2)) true else false
        case Gt => if (toStr(v1) > toStr(v2)) true else false
        case Ge => if (toStr(v1) >= toStr(v2)) true else false
      }
      case _ => bop match {
        case Lt => if (toNumber(v1) < toNumber(v2)) true else false
        case Le => if (toNumber(v1) <= toNumber(v2)) true else false
        case Gt => if (toNumber(v1) > toNumber(v2)) true else false
        case Ge => if (toNumber(v1) >= toNumber(v2)) true else false
      }
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */
  
  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eval(env, e1)), e2)

      case Call(e1, e2) => e1 match {
        case Function(p, x, e1) => eval(extend(env, x, eval(env, e2)), e1)

        case _ => DynamicTypeError(e1); Undefined
      }
        //require (but maybe not use require() e1 to be a Function, otherwise typeerror
        //evaluate e1 to v1, where v1 is the body of a function
        //then evaluate e2 to v2, where v2 is the argument to the function, mapped to env
        //then use the function v1 with the argument v2

      case Binary(bop, e1, e2) => bop match {
        case And => eval(env, e1) match {
          case N(n) => if (n == 0) N(0) else eval(env, e2)
          case B(b) => if (b == true) eval(env, e2) else B(false)
          case S(s) => eval(env, e2)
          case Undefined => Undefined
          case _ => eval(env, e2) //this doesn't do anything important, but I kept getting warnings
                                  //if nothing matches, just evaluate e2, I guess
        }
        case Or => eval(env, e1) match {
          case N(n) => if (n == 0) eval(env, e2) else N(n)
          case B(b) => if (b) B(true) else eval(env, e2)
          case S(s) => S(s)
          case Undefined => eval(env, e2)
          case _ => eval(env, e2)
        }

        case Plus => eval(env, e1) match {
          case S(s) => S(toStr(eval(env, e1)) + toStr(eval(env,e2)))
          case _ => eval(env, e2) match {
            case S(s) => S(toStr(eval(env, e1)) + toStr(eval(env,e2)))
            case _ => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
          }
        }

        case Minus => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))

        case Times => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))
        case Div => if (toNumber(eval(env, e2)) == 0) N(Double.PositiveInfinity) else N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))

        case Eq => if (eval(env, e1) == eval(env, e2)) B(true) else B(false)
        case Ne => if (eval(env, e1) != eval(env, e2)) B(true) else B(false)

        case Seq => eval(env, e1); eval(env, e2)

        case _ => B(inequalityVal(bop, e1, e2))
      }

      case Unary(uop, e1) => uop match {
        case Neg => N(-toNumber(eval(env, e1)))

        case Not => eval(env, e1) match {
          case S(s) => B(false)
          case B(b) => if (b) B(false) else B(true)
          case N(n) => if (n > 0) B(false) else B(true)
          case Undefined => B(true)
          case _ => Undefined
        }
      }

      case If(e1, e2, e3) => if (toBoolean(eval(env, e1))) eval(env, e2) else eval(env, e3)
    }
  }
    

  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = { //don't worry about what next does
    def loop(e: Expr, n: Int): Expr = next(e, n) match {
      case None => e
      case Some(e1) => loop(e1, n + 1)
    } //call next until we get None
    loop(e0, 0)
  }
  
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => ???
      case Binary(bop, e1, e2) => ???
      case If(e1, e2, e3) => ???
      case Call(e1, e2) => ???
      case Var(y) => ???
      case Function(None, y, e1) => ???
      case Function(Some(y1), y2, e1) => ???
      case ConstDecl(y, e1, e2) => ???
    }
  }
    
  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      
        // ****** Your cases here
      
      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      
        // ****** Your cases here

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */
  
  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
