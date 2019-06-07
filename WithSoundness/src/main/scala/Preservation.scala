//This file does an infinite verification with stainless even if it tests only a basic expression such as Plus

import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._

import stainless.lang._
import stainless.proof._

object Preservation{

	/**
	 * Proves a the preservation lemma.
	 * Preservation can be stated as: 
	 * "If a program type checks and makes one [next] step [with the Interpreter],
	 * then the result again type checks"
	 *
	 * @Warning this method cannot be verified within Stainless
	 */
	def preservation(e1: Expr, t: Type): Boolean ={
		require(typecheck(e1, Map[Identifier, Type]()) == Some(t) && next(e1).nonEmpty)
		val e2 = next(e1).get
		e1 match {
			case Plus(lhs, rhs) => {
				check((t == IntegerType() || t == RealType()) &&
					(typecheck(lhs, Map[Identifier, Type]()) == Some(t)) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(i1), IntegerLiteral(i2)) => check(e2 == IntegerLiteral(i1 + i2))
					case (FractionLiteral((n1, d1)), FractionLiteral((n2, d2))) => 
						check(e2 == FractionLiteral((n1 * d2 + n2 * d1, d1 * d2)))
					case (IntegerLiteral(_), _) => check(preservation(rhs, t))
					case (FractionLiteral(_), _) => check(preservation(rhs, t))
					case (_, _) => check(preservation(lhs, t))
				}
			}
		}
		typecheck(e2, Map[Identifier, Type]()) == Some(t)
	}.holds
}