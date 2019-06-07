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
          assert(typecheck(e_==(S("test"), B(true)), Map[Identifier, Type]()) === Some(BooleanType()))
        	assert(typecheck(e_>(I(1), I(2)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_>(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_>(C('c'), C('d')), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_>=(I(1), I(2)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_>=(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_>=(C('c'), C('d')), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<(I(1), I(2)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<(C('c'), C('d')), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<=(I(1), I(2)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<=(F(1, 1), F(1, 1)), Map[Identifier, Type]()) === Some(BooleanType()))
          assert(typecheck(e_<=(C('c'), C('d')), Map[Identifier, Type]()) === Some(BooleanType()))
      }
      test("Does not type checks") {
        	assert(typecheck(e_>(I(1), C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(e_<(I(1), F(1,2)), Map[Identifier, Type]()) === None())
          assert(typecheck(e_>=(S("test"), S("test1")), Map[Identifier, Type]()) === None())
          assert(typecheck(e_<=(I(1), C('c')), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          assert(interpret(e_==(S("test"), B(true))) === B(false))
          assert(interpret(e_==(I(1), I(1))) === B(true))
          assert(interpret(e_>(I(1), I(2))) === B(false))
          assert(interpret(e_>(F(1, 1), F(1, 1))) === B(false))
          assert(interpret(e_>(C('d'), C('c'))) === B(true))
          assert(interpret(e_>=(I(1), I(2))) === B(false))
          assert(interpret(e_>=(F(1, 1), F(1, 1))) === B(true))
          assert(interpret(e_>=(C('d'), C('c'))) === B(true))
          assert(interpret(e_<(I(1), I(2))) === B(true))
          assert(interpret(e_<(F(1, 1), F(1, 1))) === B(false))
          assert(interpret(e_<(C('d'), C('c'))) === B(false))
          assert(interpret(e_<=(I(1), I(2))) === B(true))
          assert(interpret(e_<=(F(1, 1), F(1, 1))) === B(true))
          assert(interpret(e_<=(C('d'), C('c'))) === B(false))
      }
      test("Next steps") {
        	assert(next(e_>(I(1), I(2))) === Some(B(false)))
          assert(next(e_>(I(1), e_+(I(2), I(3)))) === Some(e_>(I(1), I(5))))
          assert(next(e_>(e_+(I(2), I(3)), e_+(I(2), I(3)))) === Some(e_>(I(5),  e_+(I(2), I(3)))))
      }
      test("Problem with interpret") {
        	assert(interpret(e_>(I(3), C('c'))).isInstanceOf[ErrorValue])
        	assert(interpret(e_<(S("test"), S("test"))).isInstanceOf[ErrorValue])
          assert(interpret(e_>=(B(true), B(false))).isInstanceOf[ErrorValue])
      }

  }