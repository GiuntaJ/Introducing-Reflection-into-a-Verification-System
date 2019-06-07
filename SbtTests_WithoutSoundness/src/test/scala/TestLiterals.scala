import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class LiteralsTest extends FunSuite {
      test("CharLiteral") {
        	assert(typecheck(C('c'), Map[Identifier, Type]()) === Some(CharType()))
        	assert(interpret(C('c')) === CharLiteral('c'))
      }
      test("IntegerLiteral") {
        	assert(typecheck(I(1), Map[Identifier, Type]()) === Some(IntegerType()))
        	assert(interpret(I(1)) === IntegerLiteral(1))
      }
      test("BooleanLiteral") {
        	assert(typecheck(B(true), Map[Identifier, Type]()) === Some(BooleanType()))
        	assert(interpret(B(true)) === BooleanLiteral(true))
      }
      test("StringLiteral") {
        	assert(typecheck(S("string"), Map[Identifier, Type]()) === Some(StringType()))
        	assert(interpret(S("string")) === StringLiteral("string"))
      }
      test("FractionLiteral") {
        	assert(typecheck(F(1,2), Map[Identifier, Type]()) === Some(RealType()))
        	assert(interpret(F(1,2)) === FractionLiteral((1,2)))
      }

  }