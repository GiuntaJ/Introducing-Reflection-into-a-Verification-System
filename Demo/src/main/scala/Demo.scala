import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._
import DSL._

import stainless.collection._
import stainless.lang._

object Demo extends App{

	val arithmetic1 : Expr = -I(2) + I(3)*I(5)
	val arithmetic2 : Expr = F(1, 2) / F(3,5)
	val arithmetic3 : Expr = I(3)%I(0)

	val stringOps1 : Expr = SubString(S("This is ") ++ S("a demo"), I(8), I(14))
	val stringOps2 : Expr = StringLength(S("a demo"))

	val comp1 : Expr = I(4) < I(2)
	val comp2 : Expr = F(3, 4) > F(2, 3)
	val comp3 : Expr = C('d') <= C('e')
	val comp4 : Expr = S("demo") ==== C('c')

	val logic1 : Expr = B(true) || !B(false)
	val logic2 : Expr = B(true) ==> (B(false) && B(true))

	val if1 : Expr = IfExpr(B(true), C('c'), C('d'))

	val let1 : Expr = let("x", IntegerType(), I(1))(id1 => 
								let("x", IntegerType(), V("x") + I(2))(id2 =>
											V("x") + I(3)))

	val lambda1 : Expr = Lambda(
		Cons((Identifier("x"), IntegerType()), Nil()), 
		V("x") + I(1))
	val lambda2 : Expr = Application(
		Lambda(Cons((Identifier("x"), IntegerType()), Nil()), V("x") + I(1)), 
		Cons(I(2), Nil()))

	




	val listArithmetic :  scala.List[(String, Expr)] = ("-2 + 3*5 : ", arithmetic1) :: ("(1/2) / (3/5) : ", arithmetic2) :: 
														("3%0 : ", arithmetic3) :: scala.Nil
	val listStringOps :  scala.List[(String, Expr)] = ("(\"This is \" ++ \"a demo\").substring(8, 14) :\n", stringOps1) :: 
														("\"a demo\".length : ", stringOps2) :: scala.Nil
	val listComp :  scala.List[(String, Expr)] = ("4 < 2 : ", comp1) :: ("(3/4) > (2/3) : ", comp2) :: 
														("'d' <= 'e' : ", comp3) :: ("\"demo\" == 'c' : ", comp4) :: scala.Nil
	val listLogic :  scala.List[(String, Expr)] = ("true || !false : ", logic1) :: ("true ==>  (false && true) : ", logic2) :: 
														scala.Nil
	val listIf :  scala.List[(String, Expr)] = ("if true then 'c' else 'd' : ", if1) :: scala.Nil
	val listLet :  scala.List[(String, Expr)] = ("let x : BigInt = 1; let x : BigInt = x + 2; x + 3 :\n", let1) :: scala.Nil
	val listLambda :  scala.List[(String, Expr)] = ("Lambda x => x + 1 :\n", lambda1) :: ("lambda x => x + 1,  lambda(2)\n", lambda2) :: 
														scala.Nil

	printSubject("######### Arithmetic ########\n", listArithmetic)
	printSubject("##### String operations #####\n", listStringOps)
	printSubject("######## Comparisons ########\n", listComp)
	printSubject("##### Logical operators #####\n", listLogic)
	printSubject("######## If-then-else #######\n", listIf)
	printSubject("############ Let ############\n", listLet)
	printSubject("########### Lambda ##########\n", listLambda)


	def printSubject(title: String, headers_exprs: scala.List[(String, Expr)])={
		println(title)
		for((header, expr) <- headers_exprs){
			printExpr(header, expr)
		}
	}


	def printExpr(header: String, expr: Expr) = {
		print(header)
		println(expr)
		print("type checking : ")
		println(typecheck(expr, Map[Identifier, Type]()))	
		print("first next step : ")
		println(next(expr))	
		print("interpretation : ")
		println(interpret(expr))
		println("\n")
	}

}