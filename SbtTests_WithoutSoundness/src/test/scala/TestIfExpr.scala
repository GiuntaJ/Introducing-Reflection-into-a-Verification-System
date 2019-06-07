import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class IfExprTest extends FunSuite {
      test("Type checks") {
    			assert(typecheck(IfExpr(B(true), I(0), I(1)), Map[Identifier, Type]()) === Some(IntegerType()))
    			assert(typecheck(IfExpr(B(false), C('c'), C('d')), Map[Identifier, Type]()) === Some(CharType()))
      }
      test("Does not type checks") {
        	assert(typecheck(IfExpr(I(1), I(0), I(1)), Map[Identifier, Type]()) === None())
    			assert(typecheck(IfExpr(B(true), I(0), C('c')), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(IfExpr(B(true), I(0), I(1))) === I(0))
          assert(interpret(IfExpr(B(false), I(0), I(1))) === I(1))
      }
      test("Next steps") {
        	assert(next(IfExpr(B(true), I(0), I(1))) === Some(I(0)))
          assert(next(IfExpr(B(false), I(0), I(1))) === Some(I(1)))
          assert(next(IfExpr(e_&&(B(true), B(false)), I(0), I(1))) === Some(IfExpr(B(false), I(0), I(1))))
      }
      test("Problem with interpret") {
        	assert(interpret(IfExpr(I(0), I(0), I(1))).isInstanceOf[ErrorValue])
      }

  }