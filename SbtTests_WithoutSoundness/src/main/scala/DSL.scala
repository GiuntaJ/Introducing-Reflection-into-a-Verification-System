import Expressions._
import Types._
import Identifiers._

object DSL{

	implicit class WrapperExpr(lhs: Expr){
		def +(rhs: Expr): Expr = Plus(lhs, rhs)
	}

	/* Literals */

	/* Chars */
	def C(c : Char) = CharLiteral(c)
	/* Integers */
	def I(i : BigInt) = IntegerLiteral(i)
	/* Booleans */
	def B(b: Boolean) = BooleanLiteral(b)
	/* Strings */
	def S(s: String) = StringLiteral(s)
	/* Fractions */
	def F(numerator: BigInt, denominator: BigInt) = FractionLiteral((numerator, denominator))

	/* Variables */

	def V(name : String): Expr = Variable(Identifier(name))

	def let(name: String, tpe: Type, value: Expr)(body: Identifier => Expr): Expr = {
		val id = Identifier(name)
		Let(id, tpe, value, body(id))
	}

	/* General arithmetic */

	def e_+(lhs: Expr, rhs: Expr) = Plus(lhs, rhs)
	def e_-(lhs: Expr, rhs: Expr) = Minus(lhs, rhs)
	def u_-(e: Expr) = UMinus(e)
	def e_*(lhs: Expr, rhs: Expr) = Times(lhs, rhs)
	def e_/(lhs: Expr, rhs: Expr) = Division(lhs, rhs)
	def e_%(lhs: Expr, rhs: Expr) = Remainder(lhs, rhs)
	def e_mod(lhs: Expr, rhs: Expr) = Modulo(lhs, rhs)

	/* String operations */

	def e_++(lhs: Expr, rhs: Expr) = StringConcat(lhs, rhs)

	/* Comparisons */

	def e_==(lhs: Expr, rhs: Expr) = Equals(lhs, rhs)
	def e_<(lhs: Expr, rhs: Expr) = LessThan(lhs, rhs)
	def e_>(lhs: Expr, rhs: Expr) = GreaterThan(lhs, rhs)
	def e_<=(lhs: Expr, rhs: Expr) = LessEquals(lhs, rhs)
	def e_>=(lhs: Expr, rhs: Expr) = GreaterEquals(lhs, rhs)

	/* Logical operators */
	
	def e_&&(lhs: Expr, rhs: Expr) = And(lhs, rhs)
	def e_||(lhs: Expr, rhs: Expr) = Or(lhs, rhs)
	def e_==>(lhs: Expr, rhs: Expr) = Implies(lhs, rhs)
	def e_!(e: Expr) = Not(e)
}