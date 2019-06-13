import Expressions._
import Identifiers._
import Types._
import Typechecker._
import Interpreter._
import DSL._

import stainless.lang._

object ArithmeticTest{

	//Arithmetic that type checks
	def typecheckPlusInteger(): Boolean = {
		typecheck(I(1) + I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckPlusReal(): Boolean = {
		typecheck(F(1, 1) + F(1, 1), Map[Identifier, Type]()) == Some(RealType())
	}.holds

	def typecheckMinusInteger(): Boolean = {
		typecheck(I(1) - I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckMinusReal(): Boolean = {
		typecheck(F(1, 1) - F(1, 1), Map[Identifier, Type]()) == Some(RealType())
	}.holds

	def typecheckUMinusInteger(): Boolean = {
		typecheck(-I(1), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckUMinusReal(): Boolean = {
		typecheck(-F(1, 1), Map[Identifier, Type]()) == Some(RealType())
	}.holds

	def typecheckTimesInteger(): Boolean = {
		typecheck(I(1) * I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckTimesReal(): Boolean = {
		typecheck(F(1, 1) * F(1, 1), Map[Identifier, Type]()) == Some(RealType())
	}.holds

	def typecheckDivisionInteger(): Boolean = {
		typecheck(I(1) / I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckDivisionReal(): Boolean = {
		typecheck(F(1, 1) / F(1, 1), Map[Identifier, Type]()) == Some(RealType())
	}.holds

	def typecheckRemainderInteger(): Boolean = {
		typecheck(I(1) % I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckModuloInteger(): Boolean = {
		typecheck(I(1) mod I(2), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	//Arithmetic that does not type check
	def typecheckPlusOther(): Boolean = {
		typecheck(I(1) + C('c'), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckMinusOther(): Boolean = {
		typecheck(I(1) - F(1,2), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckUMinusOther(): Boolean = {
		typecheck(-C('c'), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckTimesOther(): Boolean = {
		typecheck(I(1) * C('c'), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckDivisionOther(): Boolean = {
		typecheck(I(1) / C('c'), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckRemainderOther(): Boolean = {
		typecheck(F(1, 1) % F(1, 1), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckModuloOther(): Boolean = {
		typecheck(F(1, 1) mod C('c'), Map[Identifier, Type]()) == None()
	}.holds

	//Test of interpret on simple arithmetic
	def simpleInterpretPlusInteger(): Boolean = {
		interpret(I(1) + I(2)) == I(3)
	}.holds

	def simpleInterpretPlusFraction(): Boolean = {
		interpret(F(1,2) + F(1,4)) == F(6, 8)
	}.holds

	def simpleInterpretMinusInteger(): Boolean = {
		interpret(I(1) - I(2)) == I(-1)
	}.holds

	def simpleInterpretMinusFraction(): Boolean = {
		interpret(F(1,2) - F(1,4)) == F(2, 8)
	}.holds

	def simpleInterpretUMinusInteger(): Boolean = {
		interpret(-I(1)) == I(-1)
	}.holds

	def simpleInterpretUMinusFraction(): Boolean = {
		interpret(-F(1, 2)) == F(-1, 2)
	}.holds

	def simpleInterpretTimesInteger(): Boolean = {
		interpret(I(3) * I(2)) == I(6)
	}.holds

	def simpleInterpretTimesFraction(): Boolean = {
		interpret(F(1,2) * F(2,6)) == F(2, 12)
	}.holds

	def simpleInterpretDivisionInteger(): Boolean = {
		interpret(I(3) / I(2)) == I(1)
	}.holds

	def simpleInterpretDivisionFraction(): Boolean = {
		interpret(F(1,2) / F(2,6)) == F(6, 4)
	}.holds

	def simpleInterpretRemainderInteger(): Boolean = {
		interpret(I(-15) % I(4)) == I(-3)
	}.holds

	def simpleInterpretModuloInteger(): Boolean = {
		interpret(I(15) mod I(-4)) == I(3)
	}.holds

	//Problem with interpret
	def divisionIntegerByZero(): Boolean = {
		interpret(I(3) / I(0)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def divisionFractionByZero(): Boolean = {
		interpret(F(3, 2) / F(0, 2)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def remainderByZero(): Boolean = {
		interpret(I(3) % I(0)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def moduloByZero(): Boolean = {
		interpret(I(3) mod I(0)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypesPlus(): Boolean = {
		interpret(I(1) + C('c')) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypesMinus(): Boolean = {
		interpret(S("test") - I(1)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypeUMinus(): Boolean = {
		interpret(-C('c')) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypeTimes(): Boolean = {
		interpret(I(3) * F(2, 3)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypeDivision(): Boolean = {
		interpret(F(2, 3) / I(3)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypeRemainder(): Boolean = {
		interpret(F(2, 3) % F(2, 3)) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

	def wrongTypeModulo(): Boolean = {
		interpret(F(2, 3) mod C('c')) match {
			case ErrorValue(_) => true
			case _ => false
		}
	}.holds

}