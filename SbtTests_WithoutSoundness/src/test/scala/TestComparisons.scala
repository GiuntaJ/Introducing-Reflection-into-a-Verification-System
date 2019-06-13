import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class ComparisonsTest extends FunSuite {
      test("Type checks") {
          assert(typecheck(S("test") ==== B(true), Map[Identifier, Type]()) === Some(BooleanType()))
        	assert(typecheck(I(1) > I(2), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(F(1, 1) > F(1, 1), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(C('c') > C('d'), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(I(1) >= I(2), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(F(1, 1) >= F(1, 1), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(C('c') >= C('d'), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(I(1) < I(2), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(F(1, 1) < F(1, 1), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(C('c') < C('d'), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(I(1) <= I(2), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(F(1, 1) <= F(1, 1), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(C('c') <= C('d'), Map[Identifier, Type]()) === Some(BooleanType()))
      }
      test("Does not type checks") {
        	assert(typecheck(I(1) > C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(I(1) < F(1,2), Map[Identifier, Type]()) === None())
          assert(typecheck(S("test") >= S("test1"), Map[Identifier, Type]()) === None())
          assert(typecheck(I(1) <= C('c'), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(S("test") ==== B(true)) === B(false))
          assert(interpret(I(1) ==== I(1)) === B(true))
          assert(interpret(I(1) > I(2)) === B(false))
          assert(interpret(F(1, 1) > F(1, 1)) === B(false))
          assert(interpret(C('d') > C('c')) === B(true))
          assert(interpret(I(1) >= I(2)) === B(false))
          assert(interpret(F(1, 1) >= F(1, 1)) === B(true))
          assert(interpret(C('d') >= C('c')) === B(true))
          assert(interpret(I(1) < I(2)) === B(true))
          assert(interpret(F(1, 1) < F(1, 1)) === B(false))
          assert(interpret(C('d') < C('c')) === B(false))
          assert(interpret(I(1) <= I(2)) === B(true))
          assert(interpret(F(1, 1) <= F(1, 1)) === B(true))
          assert(interpret(C('d') <= C('c')) === B(false))
      }
      test("Next steps") {
        	assert(next(I(1) > I(2)) === Some(B(false)))
          assert(next(I(1) > (I(2) + I(3))) === Some(I(1) > I(5)))
          assert(next((I(2) + I(3)) > (I(2) + I(3))) === Some(I(5) >  (I(2) + I(3))))
      }
      test("Problem with interpret") {
        	assert(interpret(I(3) > C('c')).isInstanceOf[ErrorValue])
        	assert(interpret(S("test") < S("test")).isInstanceOf[ErrorValue])
          assert(interpret(B(true) >= B(false)).isInstanceOf[ErrorValue])
      }

  }