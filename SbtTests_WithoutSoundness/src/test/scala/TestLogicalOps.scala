import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class LogicalOpsTest extends FunSuite {
      test("Type checks") {
          assert(typecheck(B(true) && B(false), Map[Identifier, Type]()) === Some(BooleanType()))
        	assert(typecheck(B(true) || B(false), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(B(true) ==> B(false), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(!B(true), Map[Identifier, Type]()) === Some(BooleanType()))
      }
      test("Does not type checks") {
        	assert(typecheck(I(1) && C('c'), Map[Identifier, Type]()) === None())
          assert(typecheck(B(false) || S("test"), Map[Identifier, Type]()) === None())
          assert(typecheck(S("test") ==> B(true), Map[Identifier, Type]()) === None())
          assert(typecheck(!I(1), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(B(true) && B(false)) === B(false))
          assert(interpret(B(true) && B(true)) === B(true))
          assert(interpret(B(true) || B(false)) === B(true))
          assert(interpret(B(false) || B(false)) === B(false))
          assert(interpret(B(false) ==> B(false)) === B(true))
          assert(interpret(B(true) ==> B(false)) === B(false))
          assert(interpret(!B(false)) === B(true))
          assert(interpret(!B(true)) === B(false))

      }
      test("Next steps") {
        	assert(next(B(true) && B(false)) === Some(B(false)))
          assert(next(B(true) && B(true)) === Some(B(true)))
          assert(next(B(false) && B(false)) === Some(B(false)))
          assert(next(B(false) && And(B(true), B(true))) === Some(B(false)))
          assert(next((B(false) && B(false)) && B(true)) === Some(B(false) && B(true)))

          assert(next(B(true) || B(false)) === Some(B(true)))
          assert(next(B(false) || B(true)) === Some(B(true)))
          assert(next(B(false) || B(false)) === Some(B(false)))
          assert(next(B(true) || (B(true) || B(true))) === Some(B(true)))
          assert(next((B(true) || B(true)) || B(true)) === Some(B(true) || B(true)))

          assert(next(B(true) ==> B(true)) === Some(B(true)))
          assert(next(B(false) ==> B(false)) === Some(B(true)))
          assert(next(B(true) ==> B(false)) === Some(B(false)))
          assert(next(B(false) ==> (B(true) ==> B(true))) === Some(B(true)))
          assert(next((B(false) ==> B(true)) ==> B(true)) === Some(B(true) ==> B(true)))

          assert(next(!B(true)) === Some(B(false)))
          assert(next(!B(false)) === Some(B(true)))
          assert(next(! (B(false) && B(false))) === Some(!B(false)))
      }
      test("Problem with interpret") {
        	assert(interpret(I(1) && C('c')).isInstanceOf[ErrorValue])
        	assert(interpret(B(false) || S("test")).isInstanceOf[ErrorValue])
          assert(interpret(S("test") ==> B(true)).isInstanceOf[ErrorValue])
          assert(interpret(!I(1)).isInstanceOf[ErrorValue])
      }

  }