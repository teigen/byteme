package bson

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

  lazy val element:Parser[(String, Value)] =
    ( 0x01 ~> ename ~ double   ^^ { case n ~ v => (n, MDouble(v)) }
    | 0x02 ~> ename ~ string   ^^ { case n ~ v => (n, MString(v)) }
    | 0x03 ~> ename ~ document ^^ { case n ~ v => (n, v) }
    | 0x04 ~> ename ~ document ^^ { case n ~ v => (n, toArray(v))}
    | 0x05 ~> ename ~ binary   ^^ { case n ~ v => (n, v) }
    | 0x07 ~> ename ~ bytes(12) ^^ { case n ~ v => (n, MObjectId(v)) }
    | 0x08 ~> ename ~ ( 0x00 ^^^ false | 0x01 ^^^ true ) ^^ { case n ~ v => (n, MBoolean(v)) }
    | 0x09 ~> ename ~ int64    ^^ { case n ~ v => (n, MDateTime(v))}
    | 0x0A ~> ename            ^^ { n => (n, MNull) }
    | 0x0B ~> ename ~ cstring ~ cstring ^^ { case n ~ v0 ~ v1 => (n, MRegexp(v0, v1))}
    | 0x0D ~> ename ~ string   ^^ { case n ~ v => (n, MCode(v)) }
    | 0x0E ~> ename ~ string   ^^ { case n ~ v => (n, MSymbol(Symbol(v))) }
    | 0x0F ~> ename ~ codeWs   ^^ { case n ~ v => (n, v) }
    | 0x10 ~> ename ~ int32    ^^ { case n ~ v => (n, MInt(v)) }
    | 0x11 ~> ename ~ int32 ~ int32  ^^ { case n ~ i ~ t => (n, MTimestamp(t, i)) }
    | 0x12 ~> ename ~ int64    ^^ { case n ~ v => (n, MLong(v)) }
    | 0xFF ~> ename            ^^ { n => (n, MinKey) }
    | 0x7F ~> ename            ^^ { n => (n, MaxKey) }
    )

  lazy val ename = cstring

  lazy val string = int32 >> (n => bytes(n - 1)) <~ 0x00 ^^ utf8

  lazy val cstring = byte.until(0x00) <~ 0x00 ^^ utf8

  lazy val binary = int32 >> { l => subtype ~ (byte * l) ^^ { case s ~ b => MBinary(s, b.toArray) } }

  lazy val subtype =
    ( 0x00 ^^^ Subtype.Generic
    | 0x01 ^^^ Subtype.Function
    | 0x03 ^^^ Subtype.Uuid
    | 0x05 ^^^ Subtype.Md5
    | 0x80 ^^^ Subtype.UserDefined
    )

  lazy val codeWs = int32 ~> string ~ document ^^ { case c ~ s => MCodeWithScope(c, s) }

  def utf8(b:Array[Byte]) = new String(b, "UTF-8")

  def toArray(document:Document) =
    MArray(document.elements.map{ case (name, value) => value })
}