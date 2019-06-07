import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class StringOpsTest extends FunSuite {
      test("Type checks") {
        	assert(typecheck(e_++(S("ab"), S("cd")), Map[Identifier, Type]()) === Some(StringType()))
			assert(typecheck(SubString(S("ab"), I(0), I(1)), Map[Identifier, Type]()) === Some(StringType()))
			assert(typecheck(StringLength(S("ab")), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
        	assert(typecheck(e_++(S("ab"), I(1)), Map[Identifier, Type]()) === None())
			assert(typecheck(e_++(I(0), S("ab")), Map[Identifier, Type]()) === None())
			assert(typecheck(SubString(S("ab"), I(0), C('c')), Map[Identifier, Type]()) === None())
			assert(typecheck(SubString(S("ab"), C('c'), I(0)), Map[Identifier, Type]()) === None())
			assert(typecheck(SubString(I(2), I(0), I(1)), Map[Identifier, Type]()) === None())
			assert(typecheck(StringLength(I(2)), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
        	assert(interpret(e_++(S("ab"), S("cd"))) === S("abcd"))
			assert(interpret(SubString(S("ab"), I(0), I(1))) === S("a"))
			assert(interpret(StringLength(S("ab"))) === I(2))
      }
      test("Next steps") {
        	assert(next(e_++(S("ab"), S("cd"))) === Some(S("abcd")))
        	assert(next(e_++(S("ab"), e_++(S("c"), S("d")))) === Some(e_++(S("ab"), S("cd"))))
        	assert(next(e_++(e_++(S("a"), S("b")), e_++(S("c"), S("d")))) === Some(e_++(S("ab"),  e_++(S("c"), S("d")))))

        	assert(next(SubString(S("ab"), I(0), I(1))) === Some(S("a")))
        	assert(next(SubString(S("ab"), I(0), e_+(I(0),I(1)))) === Some(SubString(S("ab"), I(0), I(1))))
        	assert(next(SubString(S("ab"), e_+(I(0),I(0)), e_+(I(0),I(1)))) === Some(SubString(S("ab"), I(0), e_+(I(0),I(1)))))
			assert(next(SubString(e_++(S("a"), S("b")), e_+(I(0),I(0)), e_+(I(0),I(1)))) === 
				Some(SubString(S("ab"), e_+(I(0),I(0)), e_+(I(0),I(1)))))

			assert(next(StringLength(S("ab"))) === Some(I(2)))
			assert(next(StringLength(e_++(S("a"), S("b")))) === Some(StringLength(S("ab"))))
      }
      test("Problem with interpret") {
        	assert(interpret(e_++(S("ab"), I(1))).isInstanceOf[ErrorValue])
        	assert(interpret(e_++(I(0), S("ab"))).isInstanceOf[ErrorValue])
			assert(interpret(SubString(S("ab"), I(0), C('c'))).isInstanceOf[ErrorValue])
			assert(interpret(SubString(S("ab"), C('c'), I(0))).isInstanceOf[ErrorValue])
			assert(interpret(SubString(I(2), I(0), I(1))).isInstanceOf[ErrorValue])
			assert(interpret(StringLength(I(2))).isInstanceOf[ErrorValue])
      }

  }