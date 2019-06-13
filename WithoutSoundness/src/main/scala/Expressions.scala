import Identifiers._
import Types._

import stainless.collection._
import stainless.lang._

object Expressions{
	abstract class Expr{

		/* General arithmetic */

		def +(rhs: Expr): Expr = Plus(this, rhs)
		def -(rhs: Expr): Expr = Minus(this, rhs)
		def unary_- = UMinus(this)
		def *(rhs: Expr) = Times(this, rhs)
		def /(rhs: Expr) = Division(this, rhs)
		def %(rhs: Expr) = Remainder(this, rhs)
		def mod(rhs: Expr) = Modulo(this, rhs)

		/* String operations */

		def ++(rhs: Expr) = StringConcat(this, rhs)

		/* Comparisons */

		def ====(rhs: Expr) = Equals(this, rhs)
		def <(rhs: Expr) = LessThan(this, rhs)
		def >(rhs: Expr) = GreaterThan(this, rhs)
		def <=(rhs: Expr) = LessEquals(this, rhs)
		def >=(rhs: Expr) = GreaterEquals(this, rhs)

		/* Logical operators */
		
		def &&(rhs: Expr) = And(this, rhs)
		def ||(rhs: Expr) = Or(this, rhs)
		def ==>(rhs: Expr) = Implies(this, rhs)
		def unary_! = Not(this)
	}

	/* Errors */
	case class ErrorValue(error: String) extends Expr

	/* Literals */
	case class CharLiteral(value: Char) extends Expr
	case class IntegerLiteral(value: BigInt) extends Expr
	case class BooleanLiteral(value: Boolean) extends Expr
	case class StringLiteral(value: String) extends Expr
	
	//The value of a fraction literal represents the (numerator, denominator)
	case class FractionLiteral(value: (BigInt, BigInt)) extends Expr

	/* Variable */
	case class Variable(id : Identifier) extends Expr
	// encoding of val id : t = value; body
	case class Let(id: Identifier, t: Type, value: Expr, body: Expr) extends Expr

	/* Lambda */
	case class Lambda(params: List[(Identifier, Type)], body: Expr) extends Expr
	// encoding of `callee(args...)`, where <callee> is an expression of a function type (lambda)
	case class Application(callee: Expr, args: List[Expr]) extends Expr

	/* Control-flow */
	case class IfExpr(cond: Expr, thenn: Expr, elze: Expr) extends Expr

	/* General arithmetic */
	case class Plus(lhs: Expr, rhs: Expr) extends Expr
	case class Minus(lhs: Expr, rhs: Expr) extends Expr
	case class UMinus(expr: Expr) extends Expr
	case class Times(lhs: Expr, rhs: Expr) extends Expr

	/** encoding of `... /  ...`
	*
	* Division and Remainder follows Java/Scala semantics. Division corresponds
	* to / operator on BigInt and Remainder corresponds to %. Note that in
	* Java/Scala % is called remainder and the "mod" operator is also
	* defined on BigInteger and differs from Remainder. The "mod" operator
	* returns an always positive remainder, while Remainder could return
	* a negative remainder. The following must hold:
	*
	*    Division(x, y) * y + Remainder(x, y) == x
	*/
	case class Division(lhs: Expr, rhs: Expr) extends Expr
	// '%' (can return negative numbers)
	case class Remainder(lhs: Expr, rhs: Expr) extends Expr
	// 'mod' (cannot return negative numbers)
	case class Modulo(lhs: Expr, rhs: Expr) extends Expr

	/* String Operations */
	case class StringConcat(lhs: Expr, rhs: Expr) extends Expr
	case class SubString(expr: Expr, start: Expr, end: Expr) extends Expr
	case class StringLength(expr: Expr) extends Expr

	/* Comparisons */
	case class Equals(lhs: Expr, rhs: Expr) extends Expr
	case class LessThan(lhs: Expr, rhs: Expr) extends Expr
	case class GreaterThan(lhs: Expr, rhs: Expr) extends Expr
	case class LessEquals(lhs: Expr, rhs: Expr) extends Expr
	case class GreaterEquals(lhs: Expr, rhs: Expr) extends Expr

	/* Propositional logic */
	case class And(lhs: Expr, rhs: Expr) extends Expr
	case class Or(lhs: Expr, rhs: Expr) extends Expr
	// '==>' logical implication
	case class Implies(lhs: Expr, rhs: Expr) extends Expr
	case class Not(expr: Expr) extends Expr

}