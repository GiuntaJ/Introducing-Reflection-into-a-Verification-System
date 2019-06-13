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
        	assert(typecheck(I(1) + I(2), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(F(1, 1) + F(1, 1), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(I(1) - I(2), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(F(1, 1) - F(1, 1), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(-I(1), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(-F(1, 1), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(I(1) * I(2), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(F(1, 1) * F(1, 1), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(I(1) / I(2), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(F(1, 1) / F(1, 1), Map[Identifier, Type]()) === Some(RealType()))
          assert(typecheck(I(1) % I(2), Map[Identifier, Type]()) === Some(IntegerType()))
          assert(typecheck(I(1) mod I(2), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
        	assert(typecheck(I(1) + C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(I(1) - F(1,2), Map[Identifier, Type]()) === None())
          assert(typecheck(-C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(I(1) * C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(I(1) / C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(F(1, 1) % F(1, 1), Map[Identifier, Type]()) === None())
          assert(typecheck(F(1, 1) mod C('c'), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
        	assert(interpret(I(1) + I(2)) === I(3))
          assert(interpret(F(1,2) + F(1,4)) === F(6, 8))
          assert(interpret(I(1) - I(2)) === I(-1))
          assert(interpret(F(1,2) - F(1,4)) === F(2, 8))
          assert(interpret(-I(2)) === I(-2))
          assert(interpret(-F(1, 2)) === F(-1, 2))
          assert(interpret(I(3) * I(2)) === I(6))
          assert(interpret(F(1,2) * F(2,6)) === F(2, 12))
          assert(interpret(I(3) / I(2)) === I(1))
          assert(interpret(F(1,2) / F(2,6)) === F(6, 4))
          assert(interpret(I(-15) % I(4)) === I(-3))
          assert(interpret(I(15) mod I(-4)) === I(3))
      }
      test("Next steps") {
        	assert(next(I(1) + I(2)) === Some(I(3)))
          assert(next(I(1) + Plus(I(2), I(3))) === Some(I(1) + I(5)))
          assert(next(Plus(I(2), I(3)) + Plus(I(2), I(3))) === Some(I(5) +  Plus(I(2), I(3))))
      }
      test("Problem with interpret") {
        	assert(interpret(I(3) / I(0)).isInstanceOf[ErrorValue])
        	assert(interpret(F(3, 2) / F(0, 2)).isInstanceOf[ErrorValue])
          assert(interpret(I(3) % I(0)).isInstanceOf[ErrorValue])
          assert(interpret(I(3) mod I(0)).isInstanceOf[ErrorValue])
          assert(interpret(I(1) + C('c')).isInstanceOf[ErrorValue])
          assert(interpret(S("test") - I(1)).isInstanceOf[ErrorValue])
          assert(interpret(-C('c')).isInstanceOf[ErrorValue])
          assert(interpret(I(3) * F(2, 3)).isInstanceOf[ErrorValue])
          assert(interpret(F(2, 3) / I(3)).isInstanceOf[ErrorValue])
          assert(interpret(F(2, 3) % F(2, 3)).isInstanceOf[ErrorValue])
          assert(interpret(F(2, 3) mod C('c')).isInstanceOf[ErrorValue])
      }

  }