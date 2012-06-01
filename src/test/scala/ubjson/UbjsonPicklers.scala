package ubjson

import byteme._
import Picklers._
import BigEndian._
import Literal.char

object UbjsonPicklers {
  
  lazy val value:Pickler[Value] = 
    ( string.wrap(UString)(_.value) 
    | 'Z' ^^^ UNull
    | ('T' ^^^ true | 'F' ^^^ false).wrap(UBoolean)(_.value)
    | 'B' ~> byte.wrap(UByte)(_.value)
    | 'i' ~> int16.wrap(UInt16)(_.value)
    | 'I' ~> int32.wrap(UInt32)(_.value)
    | 'L' ~> int64.wrap(UInt64)(_.value)
    | 'd' ~> float.wrap(UFloat)(_.value)
    | 'D' ~> double.wrap(UDouble)(_.value)
    | (length('h', 'H') ^^ strings.utf8).wrap(s => UHuge(BigDecimal(s)))(_.value.toString)
    | container )

  lazy val string = 
    length('s', 'S') ^^ strings.utf8
  
  lazy val container:Pickler[Value] = 
    ( count('o', 'O', field).wrap(UObject)(_.fields)
    | count('a', 'A', value).wrap(UArray)(_.values))
  
  lazy val field = (string ~ value).wrap{ case a ~ b => (a, b) }{ case (a, b) => new ~(a, b) }
  
  def count[A](small:Char, large:Char, pickler:Pickler[A]):Pickler[List[A]] =
    (small ~> byte.unsigned | large ~> int32) * pickler
  
  def length(small:Char, large:Char):Pickler[Array[Byte]] =
    (small ~> byte.unsigned | large ~> int32).bytes
}
