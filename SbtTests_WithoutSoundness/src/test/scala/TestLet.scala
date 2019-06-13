import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class LetTest extends FunSuite {
      test("Type checks") {
          assert(typecheck(let("x", IntegerType(), I(1))(id => I(2) + Variable(id)), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
          assert(typecheck(let("x", CharType(), I(1))(id => I(2) + Variable(id)), Map[Identifier, Type]()) === None())
          assert(typecheck(let("x", IntegerType(), C('c'))(id => I(2) + Variable(id)), Map[Identifier, Type]()) === None())
          assert(typecheck(let("x", IntegerType(), I(1))(id => C('c') + Variable(id)), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(let("x", IntegerType(), I(1))(id => I(2) + Variable(id))) === I(3))
      }
      test("Double let interpret"){
          assert(interpret(let("x", IntegerType(), I(1))(id => 
            let("x", IntegerType(), I(2) + Variable(id))(id2 => Variable(id) + Variable(id2)))) === I(6))
          assert(interpret(let("x", IntegerType(), I(1))(id => 
            let("y", IntegerType(), I(2) + Variable(id))(id2 => Variable(id) + Variable(id2)))) === I(4))
      }
      test("Next steps") {
          assert(next(let("x", IntegerType(), I(1))(id => I(2))) === Some(I(2)))
          assert(next(let("x", IntegerType(), I(1))(id => I(2) + Variable(id))) === Some(I(2) + I(1)))
          assert(next(let("x", IntegerType(), I(1) + I(2))(id => I(2) + Variable(id))) === 
            Some(let("x", IntegerType(), I(3))(id => I(2) + Variable(id))))
      }
      test("Problem with interpret") {
          assert(interpret(let("x", IntegerType(), C('c'))(id => I(2) + Variable(id))).isInstanceOf[ErrorValue])
          assert(interpret(let("x", IntegerType(), I(1))(id => C('c') + Variable(id))).isInstanceOf[ErrorValue])
      }

  }