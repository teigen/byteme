package ubjson

import byteme._
import Output._
import BigEndian._

object UbjsonOutput {
  
  def value(v:Value):Output = v match {
    case UString(s)      => string(s)
    case UNull           => byte('Z')
    case UBoolean(true)  => byte('T')
    case UBoolean(false) => byte('F')
    case UByte(b)        => byte('B') ++ byte(b)
    case UInt16(s)       => byte('i') ++ int16(s)
    case UInt32(i)       => byte('I') ++ int32(i)
    case UInt64(l)       => byte('L') ++ int64(l)
    case UFloat(f)       => byte('d') ++ float(f)
    case UDouble(d)      => byte('D') ++ double(d)
    case UHuge(h)        => length('h', 'H', array(h.toString().getBytes))
    case UObject(f)      => count('o', 'O', f.size) ++ (Output * field)(f)
    case UArray(a)       => count('a', 'A', a.size) ++ (Output * value)(a)
  }
  
  def field(f:(String, Value)) = string(f._1) ++ value(f._2)
  
  def string(s:String) =
    length('s', 'S', array(s.getBytes("UTF-8")))
  
  def count(small:Byte, high:Byte, count:Int) = 
    if(count < 256) 
      byte(small) ++ byte.unsigned(count) 
    else 
      byte(high) ++ int32(count) 
  
  def length(small:Byte, high:Byte, out:Output) = {
    val marker = if(out.length < 256)
      byte(small) ++ byte.unsigned(out.length)
    else
      byte(high) ++ int32(out.length)
    marker ++ out
  }
}
