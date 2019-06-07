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
          assert(typecheck(e_&&(B(true), B(false)), Map[Identifier, Type]()) === Some(BooleanType()))
        	assert(typecheck(e_||(B(true), B(false)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_==>(B(true), B(false)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_!(B(true)), Map[Identifier, Type]()) === Some(BooleanType()))
      }
      test("Does not type checks") {
        	assert(typecheck(e_&&(I(1), C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_||(B(false), S("test")), Map[Identifier, Type]()) === None())
          assert(typecheck(e_==>(S("test"), B(true)), Map[Identifier, Type]()) === None())
          assert(typecheck(e_!(I(1)), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(e_&&(B(true), B(false))) === B(false))
          assert(interpret(e_&&(B(true), B(true))) === B(true))
          assert(interpret(e_||(B(true), B(false))) === B(true))
          assert(interpret(e_||(B(false), B(false))) === B(false))
          assert(interpret(e_==>(B(false), B(false))) === B(true))
          assert(interpret(e_==>(B(true), B(false))) === B(false))
          assert(interpret(e_!(B(false))) === B(true))
          assert(interpret(e_!(B(true))) === B(false))

      }
      test("Next steps") {
        	assert(next(e_&&(B(true), B(false))) === Some(B(false)))
          assert(next(e_&&(B(true), B(true))) === Some(B(true)))
          assert(next(e_&&(B(false), B(false))) === Some(B(false)))
          assert(next(e_&&(B(false), e_&&(B(true), B(true)))) === Some(B(false)))
          assert(next(e_&&(e_&&(B(false), B(false)), B(true))) === Some(e_&&(B(false), B(true))))

          assert(next(e_||(B(true), B(false))) === Some(B(true)))
          assert(next(e_||(B(false), B(true))) === Some(B(true)))
          assert(next(e_||(B(false), B(false))) === Some(B(false)))
          assert(next(e_||(B(true), e_||(B(true), B(true)))) === Some(B(true)))
          assert(next(e_||(e_||(B(true), B(true)), B(true))) === Some(e_||(B(true), B(true))))

          assert(next(e_==>(B(true), B(true))) === Some(B(true)))
          assert(next(e_==>(B(false), B(false))) === Some(B(true)))
          assert(next(e_==>(B(true), B(false))) === Some(B(false)))
          assert(next(e_==>(B(false), e_==>(B(true), B(true)))) === Some(B(true)))
          assert(next(e_==>(e_==>(B(false), B(true)), B(true))) === Some(e_==>(B(true), B(true))))

          assert(next(e_!(B(true))) === Some(B(false)))
          assert(next(e_!(B(false))) === Some(B(true)))
          assert(next(e_!(e_&&(B(false), B(false)))) === Some(e_!(B(false))))
      }
      test("Problem with interpret") {
        	assert(interpret(e_&&(I(1), C('c'))).isInstanceOf[ErrorValue])
        	assert(interpret(e_||(B(false), S("test"))).isInstanceOf[ErrorValue])
          assert(interpret(e_==>(S("test"), B(true))).isInstanceOf[ErrorValue])
          assert(interpret(e_!(I(1))).isInstanceOf[ErrorValue])
      }

  }