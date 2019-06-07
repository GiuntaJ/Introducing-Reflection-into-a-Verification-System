import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.lang._

class LambdaTest extends FunSuite {
      
      test("Type checks") {
          val lambda1 = Lambda1(Identifier("x"), IntegerType(), Plus(V("x"), I(2)))
          val lambda2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), IntegerType(), Plus(V("x"), V("y")))

          assert(typecheck(lambda1, Map[Identifier, Type]()) === Some(FunctionType1(IntegerType(), IntegerType())))
          assert(typecheck(Application1(lambda1, I(1)), Map[Identifier, Type]()) === Some(IntegerType()))

          assert(typecheck(lambda2, Map[Identifier, Type]()) === Some(FunctionType2(IntegerType(), IntegerType(), IntegerType())))
          assert(typecheck(Application2(lambda2, I(1), I(2)), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
          val lambda1 = Lambda1(Identifier("x"), CharType(), Plus(V("x"), I(2)))
          val lambda2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), CharType(), Plus(V("x"), V("y")))

          assert(typecheck(lambda1, Map[Identifier, Type]()) === None())
          assert(typecheck(lambda2, Map[Identifier, Type]()) === None())
          assert(typecheck(Application1(lambda1, C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(Application2(lambda2, I(1), C('c')), Map[Identifier, Type]()) === None())
          
          val lambda1_2 = Lambda1(Identifier("x"), IntegerType(), Plus(V("x"), I(2)))
          val lambda2_2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), IntegerType(), Plus(V("x"), V("y")))

          assert(typecheck(Application1(lambda1_2, C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(Application2(lambda2_2, I(2), C('c')), Map[Identifier, Type]()) === None())

          assert(typecheck(Application1(lambda2_2, C('c')), Map[Identifier, Type]()) === None())
          assert(typecheck(Application2(lambda1_2, I(2), C('c')), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          val lambda1 = Lambda1(Identifier("x"), IntegerType(), Plus(V("x"), I(2)))
          val lambda2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), IntegerType(), Plus(V("x"), V("y")))

          assert(interpret(lambda1) === lambda1)
          assert(interpret(lambda2) === lambda2)
          assert(interpret(Application1(lambda1, I(1))) === I(3))
          assert(interpret(Application2(lambda2, I(1), I(2))) === I(3))
      }
      test("Next steps") {
          val lambda1 = Lambda1(Identifier("x"), IntegerType(), Plus(V("x"), I(2)))
          val lambda2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), IntegerType(), Plus(V("x"), V("y")))

          assert(next(lambda1) === None())
          assert(next(lambda2) === None())

          assert(next(Application1(lambda1, I(1))) === Some(Plus(I(1), I(2))))
          assert(next(Application1(lambda1, Plus(I(1), I(2)))) === Some(Application1(lambda1, I(3))))

          assert(next(Application1(let("x", IntegerType(), I(10))(id => lambda1), Plus(I(1), I(2)))) === Some(Application1(lambda1, Plus(I(1), I(2)))))

          assert(next(Application2(lambda2, I(1), I(2))) === Some(Plus(I(1), I(2))))
          assert(next(Application2(lambda2, I(1), Plus(I(1), I(2)))) === Some(Application2(lambda2, I(1), I(3))))
          assert(next(Application2(lambda2, Plus(I(1), I(3)), Plus(I(1), I(2)))) === Some(Application2(lambda2, I(4), Plus(I(1), I(2)))))

          assert(next(Application2(let("x", IntegerType(), I(10))(id => lambda2), Plus(I(1), I(3)), Plus(I(1), I(2)))) === 
            Some(Application2(lambda2, Plus(I(1), I(3)), Plus(I(1), I(2)))))

      }
      test("Problem with interpret") {
          val lambda1 = Lambda1(Identifier("x"), CharType(), Plus(V("x"), I(2)))
          val lambda2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), CharType(), Plus(V("x"), V("y")))

          assert(interpret(Application1(lambda1, C('c'))).isInstanceOf[ErrorValue])
          assert(interpret(Application2(lambda2, I(1), C('c'))).isInstanceOf[ErrorValue])
          
          val lambda1_2 = Lambda1(Identifier("x"), IntegerType(), Plus(V("x"), I(2)))
          val lambda2_2 = Lambda2(Identifier("x"), IntegerType(), Identifier("y"), IntegerType(), Plus(V("x"), V("y")))

          assert(interpret(Application1(lambda1_2, C('c'))).isInstanceOf[ErrorValue])
          assert(interpret(Application2(lambda2_2, I(1), C('c'))).isInstanceOf[ErrorValue])

          
          assert(interpret(Application1(lambda2_2, C('c'))).isInstanceOf[ErrorValue])
          assert(interpret(Application2(lambda1_2, I(1), C('c'))).isInstanceOf[ErrorValue])
      }

  }