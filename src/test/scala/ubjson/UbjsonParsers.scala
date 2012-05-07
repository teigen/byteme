package ubjson

import byteme._
import Parsers._
import BigEndian._
import Literal.char

/**
 * implementation of http://ubjson.org/
 */
object UbjsonParsers {

  /**
   * [type, 1-byte char]([length, 1 or 4-byte integer])([data])
   */
  lazy val value:Parser[Value] =
    ( string ^^ UString
    | 'Z' ^^^ UNull
    | 'T' ^^^ UBoolean(true)
    | 'F' ^^^ UBoolean(false)
    | 'B' ~>! byte   ^^ UByte
    | 'i' ~>! int16  ^^ UInt16
    | 'I' ~>! int32  ^^ UInt32
    | 'L' ~>! int64  ^^ UInt64
    | 'd' ~>! float  ^^ UFloat
    | 'D' ~>! double ^^ UDouble
    | length('h', 'H') >> (byte *) ^^ { b => UHuge(BigDecimal(utf8(b))) }
    | container )

  lazy val string =
    length('s', 'S') >> (byte *) ^^ utf8

  /**
   * [type, 1-byte char]([count, 1 or 4-byte integer])([data])
   */
  lazy val container =
    ( count('o', 'O') >> (field *) ^^ UObject
    | count('a', 'A') >> (value *) ^^ UArray )

  lazy val field =
    string ~ value ^^ { case a ~ b => (a, b) }

  private def length(b:Char, i:Char) =
    b ~>! byte.unsigned | i ~>! int32

  private def count(b:Char, i:Char) =
    b ~>! byte.unsigned | i ~>! int32

  private def utf8(b:List[Byte]) =
    new String(b.toArray, "UTF-8")
}