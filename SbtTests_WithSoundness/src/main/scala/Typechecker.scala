import Types._
import Expressions._
import Identifiers._

import stainless.collection._
import stainless.lang._

object Typechecker{
	/**
	 * Type checks an expression in a recursive manner
	 * given a type environment
	 *
	 * @param expr, the expression to type check
	 * @param env, an environment mapping Identifier to Type
	 * @return None() if the expression expr does not type check or
	 * an Option containing the final type of the given expression
	 * if it type checks.
	 */
	def typecheck(expr: Expr, env: Map[Identifier, Type]): Option[Type] = 
		expr match {

			//Literals
			case CharLiteral(_) => Some(CharType())
			case IntegerLiteral(_) => Some(IntegerType())
			case BooleanLiteral(_) => Some(BooleanType())
			case StringLiteral(_) => Some(StringType())
			case FractionLiteral(_) => Some(RealType())

			//Variable
			case Variable(id) => env.get(id)

			case Let(id, t :Type, value, body) => {
				if(typecheck(value, env) == Some(t)) typecheck(body, env.updated(id, t))
				else None()
			}

			//Control-flow
			case IfExpr(cond, thenn, elze) => {
				if(typecheck(cond, env) == Some(BooleanType())) {
					val (tp1, tp2) = (typecheck(thenn, env), typecheck(elze, env))
					(tp1, tp2) match {
						case (Some(t1), Some(t2)) if (t1 == t2) => Some(t1)
						case _ => None()
					}
				} else None()
			}

			//Lambda
			case Lambda1(id, t, body) => {
				typecheck(body, env updated (id, t)).map(FunctionType1(t, _))
			}
			case Lambda2(id1, t1, id2, t2, body) => {
				val newEnv = env updated (id1, t1)
				typecheck(body, newEnv updated (id2, t2)).map(FunctionType2(t1, t2, _))
			}

			case Application1(callee, arg) => 
				typecheck(callee, env) match {
					case Some(FunctionType1(from, to)) => 
						if (typecheck(arg, env) == Some(from)) 
						Some(to) else None()
					case _ => None() 
				}
			case Application2(callee, arg1, arg2) => 
				typecheck(callee, env) match {
					case Some(FunctionType2(from1, from2, to)) => 
						if (typecheck(arg1, env) == Some(from1) &&
							typecheck(arg2, env) == Some(from2)) 
						Some(to) else None()
					case _ => None() 
				}

			//General arithmetic
			case Plus(lhs: Expr, rhs: Expr) => 
				areBothIntOrReal(typecheck(lhs, env), typecheck(rhs, env))
			case Minus(lhs: Expr, rhs: Expr) => 
				areBothIntOrReal(typecheck(lhs, env), typecheck(rhs, env))
			case UMinus(e: Expr) => {
				val t = typecheck(e, env)
				areBothIntOrReal(t, t)
			}
			case Times(lhs: Expr, rhs: Expr) =>
				areBothIntOrReal(typecheck(lhs, env), typecheck(rhs, env))
			case Division(lhs: Expr, rhs: Expr) => 
				areBothIntOrReal(typecheck(lhs, env), typecheck(rhs, env))
			case Remainder(lhs: Expr, rhs: Expr) => 
				areBothInt(typecheck(lhs, env), typecheck(rhs, env))
			case Modulo(lhs: Expr, rhs: Expr) => 
				areBothInt(typecheck(lhs, env), typecheck(rhs, env))

			//String operations
			case StringConcat(lhs: Expr, rhs: Expr) => 
				if (typecheck(lhs, env) == Some(StringType()) && 
					typecheck(rhs, env) == Some(StringType()))
				 	Some(StringType()) else None()
			case SubString(e: Expr, start: Expr, end: Expr) => 
				if (typecheck(e, env) == Some(StringType()) &&
					typecheck(start, env) == Some(IntegerType()) &&
					typecheck(end, env) == Some(IntegerType())) 
					Some(StringType()) else None()
			case StringLength(e: Expr) => 
				if (typecheck(e, env) == Some(StringType())) Some(IntegerType()) else None()

			//Comparisons
			case Equals(lhs: Expr, rhs: Expr) => 
				(typecheck(lhs, env), typecheck(rhs, env)) match{
					case (Some(t1), Some(t2)) => Some(BooleanType())
					case _ => None()
				}
			case LessThan(lhs: Expr, rhs: Expr) => 
				areBothIntOrRealOrChar(typecheck(lhs, env), typecheck(rhs, env))
			case GreaterThan(lhs: Expr, rhs: Expr) =>
				areBothIntOrRealOrChar(typecheck(lhs, env), typecheck(rhs, env))
			case LessEquals(lhs: Expr, rhs: Expr)  =>
				areBothIntOrRealOrChar(typecheck(lhs, env), typecheck(rhs, env))
			case GreaterEquals(lhs: Expr, rhs: Expr)  =>
				areBothIntOrRealOrChar(typecheck(lhs, env), typecheck(rhs, env))

			//Propositional logic
			case And(lhs: Expr, rhs: Expr) => 
				areBothBoolean(typecheck(lhs, env), typecheck(rhs, env))
			case Or(lhs: Expr, rhs: Expr) => 
				areBothBoolean(typecheck(lhs, env), typecheck(rhs, env))
			case Implies(lhs: Expr, rhs: Expr) => 
				areBothBoolean(typecheck(lhs, env), typecheck(rhs, env))
			case Not(e: Expr) => {
				val t = typecheck(e, env)
				areBothBoolean(t, t)
			}


			case _ => None()
		}

	/**
	 * Check if two given Option of Type contain the type
	 * BooleanType.
	 *
	 * @param t1, the first Option of Type
	 * @param t2, the second Option of Type
	 * @return None() if they do not contain both BooleanType, 
	 * otherwise return Some(BooleanType()).
	 */
	def areBothBoolean(t1 : Option[Type], t2: Option[Type]) : Option[Type]= {
		(t1, t2) match{
			case (Some(BooleanType()), Some(BooleanType())) => Some(BooleanType())
			case _ => None()
		}
	}

	/**
	 * Check if two given Option of Type contain the type
	 * IntegerType.
	 *
	 * @param t1, the first Option of Type
	 * @param t2, the second Option of Type
	 * @return None() if they do not contain both IntegerType, 
	 * otherwise return Some(IntegerType()).
	 */
	def areBothInt(t1 : Option[Type], t2: Option[Type]) : Option[Type]= {
		(t1, t2) match{
			case (Some(IntegerType()), Some(IntegerType())) => Some(IntegerType())
			case _ => None()
		}
	}

	/**
	 * Check if two given Option of Type contain the type
	 * IntegerType or RealType.
	 *
	 * @param t1, the first Option of Type
	 * @param t2, the second Option of Type
	 * @return None() if they do not contain both either IntegerType
	 * or either RealType, 
	 * otherwise return an Option containing the type they both have.
	 */
	def areBothIntOrReal(t1 : Option[Type], t2: Option[Type]) : Option[Type]= {
		(t1, t2) match{
			case (Some(IntegerType()), Some(IntegerType())) => Some(IntegerType())
			case (Some(RealType()), Some(RealType())) => Some(RealType())
			case _ => None()
		}
	}

	/**
	 * Check if two given Option of Type contain the type
	 * IntegerType, RealType or CharType.
	 *
	 * @param t1, the first Option of Type
	 * @param t2, the second Option of Type
	 * @return None() if they do not contain both either IntegerType
	 * either RealType or either CharType, 
	 * otherwise return Some(BooleanType()).
	 */
	def areBothIntOrRealOrChar(t1 : Option[Type], t2: Option[Type]) : Option[Type]= {
		(t1, t2) match{
			case (Some(IntegerType()), Some(IntegerType())) => Some(BooleanType())
			case (Some(RealType()), Some(RealType())) => Some(BooleanType())
			case (Some(CharType()), Some(CharType())) => Some(BooleanType())
			case _ => None()
		}
	}
}