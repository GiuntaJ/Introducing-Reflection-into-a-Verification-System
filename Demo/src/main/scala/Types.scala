import stainless.collection._

object Types{
	abstract class Type

	//Base types
	case class BooleanType() extends Type
	case class CharType()    extends Type
	case class IntegerType() extends Type
	case class StringType()  extends Type
	case class RealType() extends Type

	case class FunctionType(from: List[Type], to: Type) extends Type
}