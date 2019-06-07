import Expressions._
import Identifiers._
import Types._
import Typechecker._
import Interpreter._
import DSL._

import stainless.lang._

object VariableTest{

	def typecheckVariableUnknown(): Boolean = {
		typecheck(V("test"), Map[Identifier, Type]()) == None()
	}.holds

	def typecheckVariable(): Boolean = {
		val map = Map[Identifier, Type]()
		typecheck(V("test"), map.updated(Identifier("test"), CharType())) == Some(CharType())
	}.holds

	def interpretVariable(): Boolean = {
		interpret(V("test")) == ErrorValue("Could not interpret the given expression")
	}.holds
}