import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import org.scalatest.FunSuite

import stainless.collection._
import stainless.lang._

class LambdaTest extends FunSuite {
      
      test("Type checks") {
          val params : List[(Identifier, Type)] = Cons((Identifier("x"), IntegerType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda = Lambda(params, Plus(V("x"), V("y")))
          val args : List[Expr] = Cons(I(2), Cons(I(3), Nil()))
          val paramsTypes : List[Type] = Cons(IntegerType(), Cons(IntegerType(), Nil()))

          assert(typecheck(lambda, Map[Identifier, Type]()) === Some(FunctionType(paramsTypes, IntegerType())))
          assert(typecheck(Application(lambda, args), Map[Identifier, Type]()) === Some(IntegerType()))
      }
      test("Does not type checks") {
          val params1 : List[(Identifier, Type)] = Cons((Identifier("x"), CharType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda1 = Lambda(params1, Plus(V("x"), V("y")))
          val args1 : List[Expr] = Cons(C('2'), Cons(I(3), Nil()))

          assert(typecheck(lambda1, Map[Identifier, Type]()) === None())
          assert(typecheck(Application(lambda1, args1), Map[Identifier, Type]()) === None())
          
          val params2 : List[(Identifier, Type)] = Cons((Identifier("x"), IntegerType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda2 = Lambda(params2, Plus(V("x"), V("y")))
          val args2 : List[Expr] = Cons(C('2'), Cons(I(3), Nil()))

          assert(typecheck(Application(lambda2, args2), Map[Identifier, Type]()) === None())

          val args3 : List[Expr] = Cons(I(3), Cons(I(2), Cons(I(1), Nil())))
          assert(typecheck(Application(lambda2, args3), Map[Identifier, Type]()) === None())
      }
      test("Simple interpret") {
          val params : List[(Identifier, Type)] = Cons((Identifier("x"), IntegerType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda = Lambda(params, Plus(V("x"), V("y")))
          val args : List[Expr] = Cons(I(2), Cons(I(3), Nil()))

          assert(interpret(lambda) === lambda)
          assert(interpret(Application(lambda, args)) === I(5))
      }
      test("Next steps") {
          val params : List[(Identifier, Type)] = Cons((Identifier("x"), IntegerType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda = Lambda(params, Plus(V("x"), V("y")))
          val args1 : List[Expr] = Cons(I(3), Cons(I(7), Nil()))
          val args2 : List[Expr] = Cons(I(3), Cons(Plus(I(3), I(4)), Nil()))
          val args3 : List[Expr] = Cons(Plus(I(1),I(2)), Cons(Plus(I(3), I(4)), Nil()))

          assert(next(lambda) === None())

          assert(next(Application(lambda, args1)) === Some(Plus(I(3), I(7))))
          assert(next(Application(lambda, args2)) === Some(Application(lambda, args1)))
          assert(next(Application(lambda, args3)) === Some(Application(lambda, args2)))

          assert(next(Application(let("x", IntegerType(), I(10))(id => lambda), args3)) === Some(Application(lambda, args3)))

      }
      test("Problem with interpret") {
          val params1 : List[(Identifier, Type)] = Cons((Identifier("x"), CharType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda1 = Lambda(params1, Plus(V("x"), V("y")))
          val args1 : List[Expr] = Cons(C('2'), Cons(I(3), Nil()))

          assert(interpret(Application(lambda1, args1)).isInstanceOf[ErrorValue])
          
          val params2 : List[(Identifier, Type)] = Cons((Identifier("x"), IntegerType()), Cons((Identifier("y"), IntegerType()), Nil()))
          val lambda2 = Lambda(params2, Plus(V("x"), V("y")))
          val args2 : List[Expr] = Cons(C('2'), Cons(I(3), Nil()))

          assert(interpret(Application(lambda2, args2)).isInstanceOf[ErrorValue])

          val args3 : List[Expr] = Cons(I(3), Cons(I(2), Cons(I(1), Nil())))

          assert(interpret(Application(lambda2, args3)).isInstanceOf[ErrorValue])
      }

  }