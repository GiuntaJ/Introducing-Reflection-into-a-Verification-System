import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class ArithmeticTest extends FunSuite {
      test("Type checks") {
        	assert(typecheck(e_+(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(e_+(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(e_-(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(e_-(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(u_-(I(1)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(u_-(F(1, 1)), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(e_*(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(e_*(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(e_/(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(e_/(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(e_%(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(e_mod(I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
        	assert(typecheck(e_+(I(1), C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_-(I(1), F(1,2)), Map[Identifier, Type]()) === None())
          assert(typecheck(u_-(C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_*(I(1), C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_/(I(1), C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_%(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === None())
          assert(typecheck(e_mod(F(1, 1), C('c')), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
        	assert(interpret(e_+(I(1), I(2))) === I(3))
          assert(interpret(e_+(F(1,2), F(1,4))) === F(6, 8))
          assert(interpret(e_-(I(1), I(2))) === I(-1))
          assert(interpret(e_-(F(1,2), F(1,4))) === F(2, 8))
          assert(interpret(u_-(I(1))) === I(-1))
          assert(interpret(u_-(F(1, 2))) === F(-1, 2))
          assert(interpret(e_*(I(3), I(2))) === I(6))
          assert(interpret(e_*(F(1,2), F(2,6))) === F(2, 12))
          assert(interpret(e_/(I(3), I(2))) === I(1))
          assert(interpret(e_/(F(1,2), F(2,6))) === F(6, 4))
          assert(interpret(e_%(I(-15), I(4))) === I(-3))
          assert(interpret(e_mod(I(15), I(-4))) === I(3))
      }
      test("Next steps") {
        	assert(next(e_+(I(1), I(2))) === Some(I(3)))
          assert(next(e_+(I(1), e_+(I(2), I(3)))) === Some(e_+(I(1), I(5))))
          assert(next(e_+(e_+(I(2), I(3)), e_+(I(2), I(3)))) === Some(e_+(I(5),  e_+(I(2), I(3)))))
      }
      test("Problem with interpret") {
        	assert(interpret(e_/(I(3), I(0))).isInstanceOf[ErrorValue])
        	assert(interpret(e_/(F(3, 2), F(0, 2))).isInstanceOf[ErrorValue])
          assert(interpret(e_%(I(3), I(0))).isInstanceOf[ErrorValue])
          assert(interpret(e_mod(I(3), I(0))).isInstanceOf[ErrorValue])
          assert(interpret(e_+(I(1), C('c'))).isInstanceOf[ErrorValue])
          assert(interpret(e_-(S("test"), I(1))).isInstanceOf[ErrorValue])
          assert(interpret(u_-(C('c'))).isInstanceOf[ErrorValue])
          assert(interpret(e_*(I(3), F(2, 3))).isInstanceOf[ErrorValue])
          assert(interpret(e_/(F(2, 3), I(3))).isInstanceOf[ErrorValue])
          assert(interpret(e_%(F(2, 3), F(2, 3))).isInstanceOf[ErrorValue])
          assert(interpret(e_mod(F(2, 3), C('c'))).isInstanceOf[ErrorValue])
      }

  }