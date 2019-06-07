import Types._
import Expressions._
import Identifiers._
import Typechecker._
import Interpreter._

import stainless.lang._
import stainless.proof._

object Progress{

	/**
	 * Proves a the progress lemma.
	 * Preservation can be stated as: 
	 * "If a program type checks, it is not stuck"
	 *
	 * @param expr, the expression on which progress is proved
	 * @param t, the type of the given expression
	 *
	 * @precondition expr is not a value and it typechecks to t
	 * @postcondition next(expr) is not None()
	 * To help prove the the postcondition, 
	 * lemmas where added for each case of expression using
	 * the method check of Stainless.
	 */
	def progress(expr: Expr, t: Type): Boolean = {
		require(!isValue(expr) && typecheck(expr, Map[Identifier, Type]()) == Some(t))
		expr match {
			//Variable
			case Let(id, tValue, value, body) => 
				value match {
					case _ if(isValue(value)) => true
					case _ => check(progress(value, tValue))
				}

			//Application (lambda)
			case Application1(callee, arg) =>
				callee match{
					case Lambda1(id, t, body) => arg match{
						case _ if(isValue(arg)) => true
						case _ => check(progress(arg, t))
					}
					case _ => {
						val tArg = typecheck(arg, Map[Identifier, Type]())
						check(tArg.nonEmpty && progress(callee, FunctionType1(tArg.get, t)))
					}
				}
			case Application2(callee, arg1, arg2) =>
				callee match{
					case Lambda2(id1, t1, id2, t2, body) => arg1 match {
						case _ if(isValue(arg1)) => arg2 match{
							case _ if(isValue(arg2)) => true
							case _ => check(progress(arg2, t2))
						}
						case _ => check(progress(arg1, t1))
					}
					case _ => {
						val tArg1 = typecheck(arg1, Map[Identifier, Type]())
						val tArg2 = typecheck(arg2, Map[Identifier, Type]())
						check(tArg1.nonEmpty && tArg2.nonEmpty && 
							progress(callee, FunctionType2(tArg1.get, tArg2.get, t)))
					}
				}

			//Control-flow
			case IfExpr(cond, thenn, elze) =>
				check(typecheck(cond, Map[Identifier, Type]()) == Some(BooleanType()) &&
					typecheck(thenn, Map[Identifier, Type]()) == Some(t) && 
					typecheck(elze, Map[Identifier, Type]()) == Some(t))
				(cond, thenn, elze) match{
					case (BooleanLiteral(true), _, _) if(isValue(thenn)) => true
					case (BooleanLiteral(true), _, _) => check(progress(thenn, t))
					case (BooleanLiteral(false), _, _) if(isValue(elze)) => true
					case (BooleanLiteral(false), _, _) => check(progress(elze, t))
					case (_, _, _) => check(progress(cond, BooleanType()))
				}

			//General Arithmetic
			case Plus(lhs, rhs) => {
				check((t == IntegerType() || t == RealType()) &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (FractionLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case Minus(lhs, rhs) => {
				check((t == IntegerType() || t == RealType()) &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (FractionLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case UMinus(e) => {
				check((t == IntegerType() || t == RealType()) &&
					typecheck(e, Map[Identifier, Type]()) == Some(t))
				e match {
					case (IntegerLiteral(_)) => true
					case (FractionLiteral(_)) => true
					case (_) => check(progress(e, t))
				}
			}
			case Times(lhs, rhs) => {
				check((t == IntegerType() || t == RealType()) &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (FractionLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case Division(lhs, rhs) => {
				check((t == IntegerType() || t == RealType()) &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (FractionLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case Remainder(lhs, rhs) => {
				check(t == IntegerType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case Modulo(lhs, rhs) => {
				check(t == IntegerType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}

			//String operations
			case StringConcat(lhs, rhs) => {
				check(t == StringType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (StringLiteral(_), StringLiteral(_)) => true
					case (StringLiteral(_), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case SubString(e, start, end) => {
				check(t == StringType() &&
					typecheck(e, Map[Identifier, Type]()) == Some(t) && 
					typecheck(start, Map[Identifier, Type]()) == Some(IntegerType()) &&
					typecheck(end, Map[Identifier, Type]()) == Some(IntegerType()))
				(e, start, end) match {
					case (StringLiteral(_), IntegerLiteral(_), IntegerLiteral(_)) => true
					case (StringLiteral(_), IntegerLiteral(_), _) => 
						check(progress(end, IntegerType()))
					case (StringLiteral(_), _, _) => 
						check(progress(start, IntegerType()))
					case (_, _, _) => check(progress(e, t))
				}
			}
			case StringLength(e) => {
				check(t == IntegerType() &&
					typecheck(e, Map[Identifier, Type]()) == Some(StringType()))
				e match {
					case (StringLiteral(_)) => true
					case (_) => check(progress(e, StringType()))
				}
			}

			//Comparisons
			case Equals(lhs, rhs) => {
				val t1 = typecheck(lhs, Map[Identifier, Type]())
				val t2 = typecheck(rhs, Map[Identifier, Type]())
				check(t == BooleanType() && t1.nonEmpty && t2.nonEmpty)
				(lhs, rhs) match {
					case (_, _) if(isValue(lhs) && isValue(rhs)) => true
					case (_, _) if(isValue(lhs)) => check(progress(rhs, t2.get))
					case (_, _) => check(progress(lhs, t1.get))
				}
			}
			case LessThan(lhs, rhs) => {
				val t1 = typecheck(lhs, Map[Identifier, Type]())
				val t2 = typecheck(rhs, Map[Identifier, Type]())
				check(t == BooleanType() && t1 == t2 &&
					(t1 == Some(IntegerType()) || 
						t1 == Some(RealType()) || 
						t1 == Some(CharType())))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (CharLiteral(_), CharLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t2.get))
					case (FractionLiteral(_), _) => check(progress(rhs, t2.get))
					case (CharLiteral(_), _) => check(progress(rhs, t2.get))
					case (_, _) => check(progress(lhs, t1.get))
				}
			} 
			case GreaterThan(lhs, rhs) =>  {
				val t1 = typecheck(lhs, Map[Identifier, Type]())
				val t2 = typecheck(rhs, Map[Identifier, Type]())
				check(t == BooleanType() && t1 == t2 &&
					(t1 == Some(IntegerType()) || 
						t1 == Some(RealType()) || 
						t1 == Some(CharType())))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (CharLiteral(_), CharLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t2.get))
					case (FractionLiteral(_), _) => check(progress(rhs, t2.get))
					case (CharLiteral(_), _) => check(progress(rhs, t2.get))
					case (_, _) => check(progress(lhs, t1.get))
				}
			} 
			case LessEquals(lhs, rhs) => {
				val t1 = typecheck(lhs, Map[Identifier, Type]())
				val t2 = typecheck(rhs, Map[Identifier, Type]())
				check(t == BooleanType() && t1 == t2 &&
					(t1 == Some(IntegerType()) || 
						t1 == Some(RealType()) || 
						t1 == Some(CharType())))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (CharLiteral(_), CharLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t2.get))
					case (FractionLiteral(_), _) => check(progress(rhs, t2.get))
					case (CharLiteral(_), _) => check(progress(rhs, t2.get))
					case (_, _) => check(progress(lhs, t1.get))
				}
			} 
			case GreaterEquals(lhs, rhs) =>  {
				val t1 = typecheck(lhs, Map[Identifier, Type]())
				val t2 = typecheck(rhs, Map[Identifier, Type]())
				check(t == BooleanType() && t1 == t2 &&
					(t1 == Some(IntegerType()) || 
						t1 == Some(RealType()) || 
						t1 == Some(CharType())))
				(lhs, rhs) match {
					case (IntegerLiteral(_), IntegerLiteral(_)) => true
					case (FractionLiteral(_), FractionLiteral(_)) => true
					case (CharLiteral(_), CharLiteral(_)) => true
					case (IntegerLiteral(_), _) => check(progress(rhs, t2.get))
					case (FractionLiteral(_), _) => check(progress(rhs, t2.get))
					case (CharLiteral(_), _) => check(progress(rhs, t2.get))
					case (_, _) => check(progress(lhs, t1.get))
				}
			}

			//Propositional logic
			case And(lhs, rhs) => {
				check(t == BooleanType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (BooleanLiteral(false), _) => true
					case (BooleanLiteral(true), BooleanLiteral(_)) => true
					case (BooleanLiteral(true), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}
			case Or(lhs, rhs) => {
				check(t == BooleanType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (BooleanLiteral(true), _) => true
					case (BooleanLiteral(false), BooleanLiteral(_)) => true
					case (BooleanLiteral(false), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}			
			case Implies(lhs, rhs) => {
				check(t == BooleanType() &&
					typecheck(lhs, Map[Identifier, Type]()) == Some(t) && 
					typecheck(rhs, Map[Identifier, Type]()) == Some(t))
				(lhs, rhs) match {
					case (BooleanLiteral(false), _) => true
					case (BooleanLiteral(true), BooleanLiteral(_)) => true
					case (BooleanLiteral(true), _) => check(progress(rhs, t))
					case (_, _) => check(progress(lhs, t))
				}
			}					
			case Not(e) => {
				check(t == BooleanType() &&
					typecheck(e, Map[Identifier, Type]()) == Some(t))
				e match {
					case (BooleanLiteral(_)) => true
					case (_) => check(progress(e, t))
				}
			}

		}

		next(expr).nonEmpty
	}.holds

}