package ubjson

sealed trait Value
case object UNull extends Value
case class UBoolean(value:Boolean) extends Value
case class UByte(value:Byte) extends Value
case class UInt16(value:Short) extends Value
case class UInt32(value:Int) extends Value
case class UInt64(value:Long) extends Value
case class UFloat(value:Float) extends Value
case class UDouble(value:Double) extends Value
case class UHuge(value:BigDecimal) extends Value
case class UString(value:String) extends Value
case class UObject(fields:List[(String, Value)]) extends Value
case class UArray(values:List[Value]) extends Value
