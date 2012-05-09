package bson

import byteme._
import Picklers._
import LittleEndian._
import Literal.int

object BsonPicklers {

  lazy val document = int32 lengthInclusive (elist <~ 0x00).wrap(Document)(_.elements)
  
  lazy val elist = element.*
  
  lazy val element:Pickler[(String, Value)] =
    ( 0x01 ~> ename ~ (double ^^ mDouble)                              ^^ tuple
    | 0x02 ~> ename ~ (string ^^ mString)                              ^^ tuple
    | 0x03 ~> ename ~ document                                         ^^ tuple
    | 0x04 ~> ename ~ (document ^^ mArray)                             ^^ tuple
    | 0x05 ~> ename ~ binary                                           ^^ tuple
    | 0x07 ~> ename ~ (byte * 12 ^^ mObjectId)                         ^^ tuple
    | 0x08 ~> ename ~ (( 0x00 ^^^ false | 0x01 ^^^ true ) ^^ mBoolean) ^^ tuple
    | 0x09 ~> ename ~ (int64 ^^ mDateTime)                             ^^ tuple
    | 0x0A ~> ename.wrap(_ -> MNull)(_._1)
    | 0x0B ~> ename ~ (cstring ~ cstring ^^ mRegexp)                   ^^ tuple
    | 0x0D ~> ename ~ (string ^^ mCode)                                ^^ tuple
    | 0x0E ~> ename ~ (string ^^ mSymbol)                              ^^ tuple
    | 0x0F ~> ename ~ codeWs                                           ^^ tuple
    | 0x10 ~> ename ~ (int32 ^^ mInt)                                  ^^ tuple
    | 0x11 ~> ename ~ (int32 ~ int32 ^^ mTimestamp)                    ^^ tuple
    | 0x12 ~> ename ~ (int64 ^^ mLong)                                 ^^ tuple
    | 0xFF ~> ename.wrap(_ -> MinKey)(_._1)
    | 0x7F ~> ename.wrap(_ -> MaxKey)(_._1)
    )
  
  lazy val ename = cstring

  lazy val cstring = byte.takeWhile(_ != 0x00) <~ 0x00 ^^ utf8
  
  lazy val string = {
    val unpickle = {
      import Parsers.Literal.int
      Parser(int32.unpickle) >> (n => (Parsers.byte * (n - 1))) <~ 0x00 ^^ utf8.wrap
    }
    
    def pickle(value:String) = {
      val out = (Output * byte.pickle)(utf8.unwrap(value)) ++ Output.byte(0x00)
      int32.pickle(out.length) ++ out
    }
    Pickler(unpickle, pickle)
  }

  lazy val binary = {
    val unpickle = 
      Parser(int32.unpickle) >> {l => Parser(subtype.unpickle) ~ (Parser(byte.unpickle) * l) } ^^ { case s ~ b => MBinary(s, b.toArray) }
    def pickle(b:MBinary) = 
      int32.pickle(b.value.length) ++ subtype.pickle(b.subtype) ++ Output.array(b.value)
    Pickler(unpickle, pickle)
  }

  lazy val subtype:Pickler[Subtype] =
    ( 0x00 ^^^ Subtype.Generic
    | 0x01 ^^^ Subtype.Function
    | 0x03 ^^^ Subtype.Uuid
    | 0x05 ^^^ Subtype.Md5
    | 0x80 ^^^ Subtype.UserDefined)

  lazy val codeWs = int32 lengthInclusive (string ~ document) ^^ mCodeWs
  
  def tuple[A, B] = Wrap[A ~ B, (A, B)]({ case a ~ b => (a, b)}, { case (a, b) => new ~(a, b) })
  
  val utf8 = collections.array[Byte] ^^ strings.utf8
  
  val mDouble    = Wrap[Double, MDouble](MDouble, _.value)
  val mString    = Wrap[String, MString](MString, _.value)
  val mArray     = Wrap[Document, MArray](toArray, toDoc)
  val mSymbol    = Wrap[String, MSymbol](s => MSymbol(Symbol(s)), _.symbol.name)
  val mCode      = Wrap[String, MCode](MCode, _.code)
  val mInt       = Wrap[Int, MInt](MInt, _.value)
  val mLong      = Wrap[Long, MLong](MLong, _.value)
  val mDateTime  = Wrap[Long, MDateTime](MDateTime, _.value)
  val mCodeWs    = Wrap[String ~ Document, MCodeWithScope]({ case c ~ s => MCodeWithScope(c, s)}, { case MCodeWithScope(c, s) => new ~(c, s) })
  val mTimestamp = Wrap[Int ~ Int, MTimestamp]({ case inc ~ time => MTimestamp(time, inc) }, { case MTimestamp(time, inc) => new ~(inc, time)})
  val mBoolean   = Wrap[Boolean, MBoolean](MBoolean, _.value)
  val mObjectId  = collections.array[Byte] ^^ Wrap[Array[Byte], MObjectId](MObjectId, _.data)
  val mRegexp    = Wrap[String ~ String, MRegexp]({ case p ~ o => MRegexp(p, o)}, { case MRegexp(p, o) => new ~(p, o) })

  def toArray(document:Document) =
    MArray(document.elements.map{ case (name, value) => value })

  def toDoc(m:MArray) =
    Document(m.values.zipWithIndex.map{ case (v, i) => (i.toString, v) })
}
