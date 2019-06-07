import Expressions._
import Identifiers._
import Types._
import Typechecker._
import Interpreter._
import DSL._

import stainless.lang._

object LiteralTest{

	//Literals with type checker
	def typecheckCharLiteral(): Boolean = {
		typecheck(C('c'), Map[Identifier, Type]()) == Some(CharType())
	}.holds

	def typecheckIntegerLiteral(): Boolean = {
		typecheck(I(1), Map[Identifier, Type]()) == Some(IntegerType())
	}.holds

	def typecheckBooleanLiteral(): Boolean = {
		typecheck(B(true), Map[Identifier, Type]()) == Some(BooleanType())
	}.holds

	def typecheckStringLiteral(): Boolean = {
		typecheck(S("string"), Map[Identifier, Type]()) == Some(StringType())
	}.holds

	def typecheckFractionLiteral(): Boolean = {
		typecheck(F(1,2), Map[Identifier, Type]()) == Some(RealType())
	}.holds


	//Interpretation of literals
	def interpretCharLiteral(): Boolean = {
		interpret(C('c')) == CharLiteral('c')
	}.holds

	def interpretIntegerLiteral(): Boolean = {
		interpret(I(1)) == IntegerLiteral(1)
	}.holds

	def interpretBooleanLiteral(): Boolean = {
		interpret(B(true)) == BooleanLiteral(true)
	}.holds

	def interpretStringLiteral(): Boolean = {
		interpret(S("string")) == StringLiteral("string")
	}.holds

	def interpretFractionLiteral(): Boolean = {
		interpret(F(1,2)) == FractionLiteral((1,2))
	}.holds
}