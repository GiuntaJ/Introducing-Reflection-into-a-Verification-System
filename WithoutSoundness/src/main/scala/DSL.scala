import Expressions._
import Types._
import Identifiers._

object DSL{

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

}