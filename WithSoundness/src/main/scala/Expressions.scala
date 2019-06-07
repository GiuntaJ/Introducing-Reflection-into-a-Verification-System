import Identifiers._
import Types._

object Expressions{
	abstract class Expr

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
	//Lambda which takes only one parameter
	case class Lambda1(id: Identifier, t: Type, body: Expr) extends Expr
	//Lambda which takes only two parameters
	case class Lambda2(id1: Identifier, t1: Type, id2: Identifier, t2: Type, body: Expr) extends Expr

	// encoding of `callee(args...)`, where <callee> is an expression of a function type (lambda)
	//For Lambda1
	case class Application1(callee: Expr, arg: Expr) extends Expr
	//For Lambda2
	case class Application2(callee: Expr, arg1: Expr, arg2: Expr) extends Expr

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