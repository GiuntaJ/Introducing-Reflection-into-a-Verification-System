import Expressions._
import Identifiers._

import stainless.lang._

object Interpreter{

	/**
	 * Check if an expression is a value (Literals, Lambda, ErrorValue)
	 * which means it can be evaluated further.
	 *
	 * @param expr, the expression to check
	 * @return true if expr is a value, false otherwise.
	 */
	def isValue(expr: Expr): Boolean = 
		expr match {
			case CharLiteral(_) => true
			case IntegerLiteral(_) => true
			case BooleanLiteral(_) => true
			case StringLiteral(_) => true
			case FractionLiteral(_) => true
			case Lambda1(_, _, _) => true
			case Lambda2(_, _, _, _, _) => true
			case ErrorValue(_) => true

			case _ => false
	}

	/**
	 * Interpret an expression using @next method recursively
	 *
	 * @param expr, the expression to interpret
	 * @return a value (see @isValue ) if the expression can be interpreted
	 * or an ErrorValue if @next was stuck.
	 */
	def interpret(expr: Expr): Expr = {
		if(isValue(expr)) expr else{
			val nextExpr = next(expr)
			nextExpr match{
				case Some(e) => interpret(e)
				case None() => ErrorValue("Could not interpret the given expression")
			}
		}

	}

	/**
	 * Does one step of the small-step interpreter.
	 *
	 * @param expr, the expression to interpret
	 * @return an Option containing the next step of the evaluation of expr
	 * or None() if the expression could not be evaluated further or 
	 * there was a problem during the interpretation.
	 */
	def next(expr: Expr): Option[Expr] =
		expr match{
			case Let(id, t, value, body) =>
				if(isValue(value)) Some(subst(body, id, value)) else
					next(value).map(Let(id, t, _, body))

			//Application (lambda)
			case Application1(callee, arg) => callee match{
				case Lambda1(id, t, body) => arg match{
					case _ if(isValue(arg)) => Some(subst(body, id, arg))
					case _ => next(arg).map(Application1(callee, _))
				}
				case _ if(isValue(callee)) => None()
				case _ => next(callee).map(Application1(_, arg))
			}
			case Application2(callee, arg1, arg2) => callee match{
				case Lambda2(id1, t1, id2, t2, body) => arg1 match{
					case _ if(isValue(arg1)) => arg2 match{
						case _ if(isValue(arg2)) => {
							Some(subst(subst(body, id1, arg1), id2, arg2))
						}
						case _ => next(arg2).map(Application2(callee, arg1, _))
					}
					case _ => next(arg1).map(Application2(callee, _, arg2))
				}
				case _ if(isValue(callee)) => None()
				case _ => next(callee).map(Application2(_, arg1, arg2))
			}

			//Control-flow
			case IfExpr(cond, thenn, elze) => cond match{
				case BooleanLiteral(true) => Some(thenn)
				case BooleanLiteral(false) => Some(elze)
				case _ if(isValue(cond)) => None()
				case _ => next(cond).map(IfExpr(_, thenn, elze))
			}

			//General Arithmetic
			case Plus(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 + i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Plus(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match {
					case FractionLiteral((n2, d2)) => Some(FractionLiteral((n1 * d2 + n2 * d1, d1 * d2)))
					case _ if(isValue(rhs)) => None()
					case _=> next(rhs).map(Plus(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Plus(_, rhs))
			}				
			case Minus(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 - i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Minus(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match {
					case FractionLiteral((n2, d2)) => Some(FractionLiteral((n1 * d2 - n2 * d1, d1 * d2)))
					case _ if(isValue(rhs)) => None()
					case _=> next(rhs).map(Minus(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Minus(_, rhs))
			}
			case UMinus(e) => e match {
				case IntegerLiteral(i) => Some(IntegerLiteral(-i))
				case FractionLiteral((n, d)) => Some(FractionLiteral((-n, d)))
				case _ if(isValue(e)) => None()
				case _ => next(e).map(UMinus(_))
			}
			case Times(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 * i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Times(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match {
					case FractionLiteral((n2, d2)) => Some(FractionLiteral((n1 * n2, d1 * d2)))
					case _ if(isValue(rhs)) => None()
					case _=> next(rhs).map(Times(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Times(_, rhs))
			}
			case Division(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(BigInt(0)) => Some(ErrorValue("Division by 0"))
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 / i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Division(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match {
					case FractionLiteral((BigInt(0), d2)) => Some(ErrorValue("Division by 0"))
					case FractionLiteral((n2, d2)) => Some(FractionLiteral((n1 * d2, d1 * n2)))
					case _ if(isValue(rhs)) => None()
					case _=> next(rhs).map(Division(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Division(_, rhs))
			}
			case Remainder(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(BigInt(0)) => Some(ErrorValue("Remainder of division by 0"))
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 % i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Remainder(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Remainder(_, rhs))
			}
			case Modulo(lhs, rhs) => lhs match{
				case IntegerLiteral(i1) => rhs match {
					case IntegerLiteral(BigInt(0)) => Some(ErrorValue("Modulo of division by 0"))
					case IntegerLiteral(i2) if(i2 < BigInt(0)) => Some(IntegerLiteral(i1 mod (-i2)))
					case IntegerLiteral(i2) => Some(IntegerLiteral(i1 mod i2))
			        case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(Modulo(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Modulo(_, rhs))
			}

			//String operations
			case StringConcat(lhs, rhs) => lhs match{
				case StringLiteral(s1) => 
					rhs match{
						case StringLiteral(s2) => Some(StringLiteral(s1 + s2))
						case _ if(isValue(rhs)) => None()
						case _ => next(rhs).map(StringConcat(lhs, _))
					}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(StringConcat(_, rhs))
			}
			case SubString(e, start, end) => e match{
				case StringLiteral(s) => 
					start match{
						case IntegerLiteral(i) => 
							end match{
								case IntegerLiteral(j) => 
									Some(StringLiteral(s.bigSubstring(i, j)))
								case _ if(isValue(end)) => None()
								case _ => next(end).map(SubString(e, start, _))
							}
						case _ if(isValue(start)) => None()
						case _ => next(start).map(SubString(e, _, end))
					}
				case _ if(isValue(e)) => None()
				case _ => next(e).map(SubString(_, start, end))
			}
			case StringLength(e) => e match {
				case StringLiteral(s) => Some(IntegerLiteral(s.bigLength))
				case _ if(isValue(e)) => None()
				case _ => next(e).map(StringLength(_))
			}

			//Comparisons
			case Equals(lhs, rhs) => 
				if(isValue(lhs)){
					if(isValue(rhs)) Some(BooleanLiteral(lhs == rhs)) else 
						next(rhs).map(Equals(lhs, _))
				}else next(lhs).map(Equals(_, rhs))
			case LessThan(lhs, rhs) => lhs match {
				case IntegerLiteral(i1) => rhs match{
					case IntegerLiteral(i2) => Some(BooleanLiteral(i1 < i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessThan(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match{
					case FractionLiteral((n2, d2)) => Some(BooleanLiteral(n1 * d2 - n2 * d1 < 0))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessThan(lhs, _))
				}
				case CharLiteral(c1) => rhs match{
					case CharLiteral(c2) => Some(BooleanLiteral(c1 < c2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessThan(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(LessThan(_, rhs))
			}
			case GreaterThan(lhs, rhs) => lhs match {
				case IntegerLiteral(i1) => rhs match{
					case IntegerLiteral(i2) => Some(BooleanLiteral(i1 > i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterThan(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match{
					case FractionLiteral((n2, d2)) => Some(BooleanLiteral(n1 * d2 - n2 * d1 > 0))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterThan(lhs, _))
				}
				case CharLiteral(c1) => rhs match{
					case CharLiteral(c2) => Some(BooleanLiteral(c1 > c2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterThan(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(GreaterThan(_, rhs))
			}
			case LessEquals(lhs, rhs) => lhs match {
				case IntegerLiteral(i1) => rhs match{
					case IntegerLiteral(i2) => Some(BooleanLiteral(i1 <= i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessEquals(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match{
					case FractionLiteral((n2, d2)) => Some(BooleanLiteral(n1 * d2 - n2 * d1 <= 0))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessEquals(lhs, _))
				}
				case CharLiteral(c1) => rhs match{
					case CharLiteral(c2) => Some(BooleanLiteral(c1 <= c2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(LessEquals(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(LessEquals(_, rhs))
			}
			case GreaterEquals(lhs, rhs) => lhs match {
				case IntegerLiteral(i1) => rhs match{
					case IntegerLiteral(i2) => Some(BooleanLiteral(i1 >= i2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterEquals(lhs, _))
				}
				case FractionLiteral((n1, d1)) => rhs match{
					case FractionLiteral((n2, d2)) => Some(BooleanLiteral(n1 * d2 - n2 * d1 >= 0))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterEquals(lhs, _))
				}
				case CharLiteral(c1) => rhs match{
					case CharLiteral(c2) => Some(BooleanLiteral(c1 >= c2))
					case _ if(isValue(rhs)) => None()
					case _ => next(rhs).map(GreaterEquals(lhs, _))
				}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(GreaterEquals(_, rhs))
			}

			//Propositional logic
			//And, Or and Implies are short circuit operators
			case And(lhs, rhs) => lhs match{
				case BooleanLiteral(false) => Some(BooleanLiteral(false))
				case BooleanLiteral(true) => 
					rhs match{
						case BooleanLiteral(true) => Some(BooleanLiteral(true))
						case BooleanLiteral(false) => Some(BooleanLiteral(false))
						case _ if(isValue(rhs)) => None()
						case _ => next(rhs).map(And(lhs, _))
					}	
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(And(_, rhs))
			}
			case Or(lhs, rhs) => lhs match{
				case BooleanLiteral(true) => Some(BooleanLiteral(true))
				case BooleanLiteral(false) => 
					rhs match{
						case BooleanLiteral(true) => Some(BooleanLiteral(true))
						case BooleanLiteral(false) => Some(BooleanLiteral(false))
						case _ if(isValue(rhs)) => None()
						case _ => next(rhs).map(Or(lhs, _))
					}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Or(_, rhs))
			}
			case Implies(lhs, rhs) => lhs match{
				case BooleanLiteral(false) => Some(BooleanLiteral(true))
				case BooleanLiteral(true) => 
					rhs match{
						case BooleanLiteral(true) => Some(BooleanLiteral(true))
						case BooleanLiteral(false) => Some(BooleanLiteral(false))
						case _ if(isValue(rhs)) => None()
						case _ => next(rhs).map(Implies(lhs, _))
					}
				case _ if(isValue(lhs)) => None()
				case _ => next(lhs).map(Implies(_, rhs))
			}
			case Not(e) => e match{
				case BooleanLiteral(b) => Some(BooleanLiteral(!b))
				case _ if(isValue(e)) => None()
				case _ => next(e).map(Not(_))
			}

			case _ => None()
		}

	/**
	 * Does a substitution of in a recursive manner.
	 *
	 * @param expr, the expression in which to substitute
	 * @param id, the identifier which need to be substituted
	 * @param value, the newly defined value of id
	 * @return the expression expr in which every occurrence of 
	 * Variable(id) was subtituted by value
	 */
	def subst(expr: Expr, id: Identifier, value: Expr): Expr =
		expr match{
			//Variable
			case Variable(vid) => if(vid == id) value else expr
			case Let(letId, t, letValue, body) => {
				val letValueSubst = subst(letValue, id, value)
				//If letId == id, letId masks id in body
				if(letId == id) Let(letId, t, letValueSubst, body) else
					Let(letId, t, letValueSubst, subst(body, id, value))
			}

			//Control-flow
			case IfExpr(cond, thenn, elze) => 
				IfExpr(subst(cond, id, value), subst(thenn, id, value), subst(elze, id, value))

			//Lambda
			case Lambda1(lid, t, body) => {
				if(lid == id) expr else Lambda1(lid, t, subst(body, id, value))
			}
			case Lambda2(lid1, t1, lid2, t2, body) => {
				if(lid1 == id || lid2 == id) expr else 
					Lambda2(lid1, t1, lid2, t2, subst(body, id, value))
			}
			case Application1(callee, arg) => {
				Application1(subst(callee, id, value), subst(arg, id, value))
			}
			case Application2(callee, arg1, arg2) => {
				Application2(subst(callee, id, value), 
					subst(arg1, id, value), subst(arg2, id, value))
			}

			//General artihmetic
			case Plus(lhs, rhs) => Plus(subst(lhs, id, value), subst(rhs, id, value))
			case Minus(lhs, rhs) => Minus(subst(lhs, id, value), subst(rhs, id, value))
			case UMinus(exp) => UMinus(subst(exp, id, value))
			case Times(lhs, rhs) => Times(subst(lhs, id, value), subst(rhs, id, value))
			case Division(lhs, rhs) => Division(subst(lhs, id, value), subst(rhs, id, value))
			case Remainder(lhs, rhs) => Remainder(subst(lhs, id, value), subst(rhs, id, value))
			case Modulo(lhs, rhs) => Modulo(subst(lhs, id, value), subst(rhs, id, value))

			//String operations
			case StringConcat(lhs, rhs) => StringConcat(subst(lhs, id, value), subst(rhs, id, value))					
			case SubString(exp, start, end) => 
					SubString(subst(exp, id, value), subst(start, id, value), subst(end, id, value))					
			case StringLength(exp) => StringLength(subst(exp, id, value))

			//Comparisons
			case Equals(lhs, rhs) => Equals(subst(lhs, id, value), subst(rhs, id, value))
			case LessThan(lhs, rhs) => LessThan(subst(lhs, id, value), subst(rhs, id, value))
			case GreaterThan(lhs, rhs) => GreaterThan(subst(lhs, id, value), subst(rhs, id, value))
			case LessEquals(lhs, rhs) => LessEquals(subst(lhs, id, value), subst(rhs, id, value))
			case GreaterEquals(lhs, rhs) => GreaterEquals(subst(lhs, id, value), subst(rhs, id, value))	

			//Propositional logic
			case And(lhs, rhs) => And(subst(lhs, id, value), subst(rhs, id, value))	
			case Or(lhs, rhs) => Or(subst(lhs, id, value), subst(rhs, id, value))			
			case Implies(lhs, rhs) => Implies(subst(lhs, id, value), subst(rhs, id, value))					
			case Not(exp) => Not(subst(exp, id, value))

			//Literals and other terminating expressions cannot be replaced
			case _ => expr
		}

}