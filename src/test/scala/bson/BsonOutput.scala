package bson

import byteme._
import Output._
import LittleEndian._
import Literal.int

object BsonOutput {
  def document(doc:Document) = {
    val out = elist(doc.elements) ++ 0x00
    int32(out.length + 4) ++ out
  } 
  
  def elist(fields:List[(String, Value)]):Output = 
    fields.foldLeft(Output.empty){ (o, e) => o ++ element(e) }
  
  def element(e:(String, Value)) = {
    val name = ename(e._1)
    e._2 match {
      case MDouble(value)   => 0x01 ++ name ++ double(value)
      case MString(value)   => 0x02 ++ name ++ string(value)
      case doc:Document     => 0x03 ++ name ++ document(doc)
      case m:MArray         => 0x04 ++ name ++ document(toDoc(m))
      case b:MBinary        => 0x05 ++ name ++ binary(b)
      case MObjectId(value) => 0x07 ++ name ++ value
      case MBoolean(value)  => 0x08 ++ name ++ (if(value) 0x01 else 0x00)
      case MDateTime(value) => 0x09 ++ name ++ int64(value)
      case MNull            => 0x0A ++ name
      case MRegexp(p, o)    => 0x0B ++ name ++ cstring(p) ++ cstring(o)
      case MCode(value)     => 0x0D ++ name ++ string(value)
      case MSymbol(value)   => 0x0E ++ name ++ string(value.name)
      case c:MCodeWithScope => 0x0F ++ name ++ codeWs(c)
      case MInt(value)      => 0x10 ++ name ++ int32(value)
      case MTimestamp(t, i) => 0x11 ++ name ++ int32(i) ++ int32(t)
      case MLong(value)     => 0x12 ++ name ++ int64(value)
      case MinKey           => 0xFF ++ name
      case MaxKey           => 0x7F ++ name
    }
  }
  
  def ename(s:String) = cstring(s)
  
  def cstring(s:String) = {
    val bytes = s.getBytes("UTF-8")
    if(bytes.contains(0x00:Byte))
      throw new IllegalArgumentException("cstrings cannot contain 0x00")
    else
      array(bytes) ++ 0x00
  }
  
  def string(s:String) = {
    val out = array(s.getBytes("UTF-8")) ++ 0x00
    int32(out.length) ++ out  
  }
  
  def binary(b:MBinary) = 
    int32(b.value.length) ++ subtype(b.subtype) ++ b.value
  
  def subtype(s:Subtype):Output = s match {
    case Subtype.Generic     => 0x00
    case Subtype.Function    => 0x01
    case Subtype.Uuid        => 0x03
    case Subtype.Md5         => 0x05
    case Subtype.UserDefined => 0x80
  }
  
  def codeWs(c:MCodeWithScope) = { 
    val out = string(c.code) ++ document(c.scope)
    int32(out.length + 4) ++ out
  }
  
  def toDoc(m:MArray) = 
    Document(m.values.zipWithIndex.map{ case (v, i) => (i.toString, v) })
}
