import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class VariableTest extends FunSuite {
      test("Variable in Typechecker and Interpreter") {
      		val map = Map[Identifier, Type]()
		
        	assert(typecheck(V("test"), map) === None())
        	assert(typecheck(V("test"), map.updated(Identifier("test"), CharType())) === Some(CharType()))
			assert(next(V("test")) === None())
        	assert(interpret(V("test")) === ErrorValue("Could not interpret the given expression"))
      }
  }