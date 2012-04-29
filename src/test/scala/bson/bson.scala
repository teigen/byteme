package bson

import java.util.Arrays
import byteme._

/**
 * implementation of http://bsonspec.org/
 */
object BsonParsers {
  import Parsers._
  import LittleEndian._
  import Literal.int

  lazy val document = int32 ~> elist <~ 0x00 ^^ Document

  lazy val elist = element.*

  lazy val element:Parser[Element] =
    ( 0x01 ~> ename ~ double   ^^ { case n ~ v => Element(n, MDouble(v)) }
    | 0x02 ~> ename ~ string   ^^ { case n ~ v => Element(n, MString(v)) }
    | 0x03 ~> ename ~ document ^^ { case n ~ v => Element(n, v) }
    | 0x04 ~> ename ~ document ^^ { case n ~ v => Element(n, toArray(v))}
    | 0x05 ~> ename ~ binary   ^^ { case n ~ v => Element(n, v) }
    | 0x07 ~> ename ~ (byte * 12) ^^ { case n ~ v => Element(n, MObjectId(v.toArray)) }
    | 0x08 ~> ename ~ ( 0x00 ^^^ false | 0x01 ^^^ true ) ^^ { case n ~ v => Element(n, MBoolean(v)) }
    | 0x09 ~> ename ~ int64 ^^ { case n ~ v => Element(n, MDateTime(v))}
    | 0x0A ~> ename ^^ { n => Element(n, MNull) }
    | 0x0B ~> ename ~ cstring ~ cstring ^^ { case n ~ v0 ~ v1 => Element(n, MRegexp(v0, v1))}
    | 0x0D ~> ename ~ string ^^ { case n ~ v => Element(n, MCode(v)) }
    | 0x0E ~> ename ~ string ^^ { case n ~ v => Element(n, MSymbol(Symbol(v))) }
    | 0x0F ~> ename ~ codeWs ^^ { case n ~ v => Element(n, v) }
    | 0x10 ~> ename ~ int32  ^^ { case n ~ v => Element(n, MInt(v)) }
    | 0x11 ~> ename ~ int32 ~ int32  ^^ { case n ~ i ~ t => Element(n, MTimestamp(t, i)) }
    | 0x12 ~> ename ~ int64  ^^ { case n ~ v => Element(n, MLong(v)) }
    | 0xFF ~> ename ^^ { n => Element(n, MinKey) }
    | 0x7F ~> ename ^^ { n => Element(n, MaxKey) }
    )

  lazy val ename = cstring

  lazy val string = int32 >> (n => byte * (n - 1)) <~ 0x00 ^^ utf8

  lazy val cstring = byte.takeWhile( _ != 0x00) <~ 0x00 ^^ utf8

  lazy val binary = int32 >> { l => subtype ~ (byte * l) ^^ { case s ~ b => MBinary(s, b.toArray) } }

  lazy val subtype =
    ( 0x00 ^^^ Subtype.Generic
    | 0x01 ^^^ Subtype.Function
    | 0x03 ^^^ Subtype.Uuid
    | 0x05 ^^^ Subtype.Md5
    | 0x80 ^^^ Subtype.UserDefined
    )

  lazy val codeWs = int32 ~> string ~ document ^^ { case c ~ s => MCodeWithScope(c, s) }
  
  def utf8(b:List[Byte]) = new String(b.toArray, "UTF-8")

  def toArray(document:Document) =
    MArray(document.elements.map{ case Element(name, value) => value })
}

sealed trait Value
case class Document(elements:List[Element]) extends Value
case class Element(name:String, value:Value)

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