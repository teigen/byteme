package bson

import java.util.Arrays

sealed trait Value
case class Document(elements:List[(String, Value)]) extends Value

case class MDouble(value:Double) extends Value
case class MString(value:String) extends Value
case class MArray(values:List[Value]) extends Value
case class MBinary(subtype:Subtype, value:Array[Byte]) extends Value{
  override def equals(any:Any) = any match {
    case MBinary(`subtype`, v) => Arrays.equals(value, v)
    case _ => false
  }
  override def hashCode = subtype.hashCode() + Arrays.hashCode(value)
}
case class MObjectId(data:Array[Byte]) extends Value {
  override def equals(any:Any) = any match {
    case MObjectId(d) => Arrays.equals(data, d)
    case _ => false
  }
  override def hashCode = Arrays.hashCode(data)
}
case class MBoolean(value:Boolean) extends Value
case class MDateTime(value:Long) extends Value
case object MNull extends Value
case class MRegexp(pattern:String, options:String) extends Value
case class MCode(code:String) extends Value
case class MSymbol(symbol:Symbol) extends Value
case class MCodeWithScope(code:String, scope:Document) extends Value
case class MInt(value:Int) extends Value
case class MTimestamp(time:Int, inc:Int) extends Value
case class MLong(value:Long) extends Value
case object MinKey extends Value
case object MaxKey extends Value

sealed trait Subtype
object Subtype {
  case object Generic extends Subtype
  case object Function extends Subtype
  case object Uuid extends Subtype
  case object Md5 extends Subtype
  case object UserDefined extends Subtype
}