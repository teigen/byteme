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
    ( count('o', 'O')(field).wrap(UObject)(_.fields)
    | count('a', 'A')(value).wrap(UArray)(_.values))
  
  lazy val field = (string ~ value).wrap{ case a ~ b => (a, b) }{ case (a, b) => new ~(a, b) }
  
  def count[A](small:Char, large:Char)(pickler:Pickler[A]):Pickler[List[A]] = {    
    val parser = 
      ( Parsers.Literal.char(small) ~> Parsers.byte.unsigned 
      | Parsers.Literal.char(large) ~> Parser(int32.unpickle)) >> (Parser(pickler.unpickle) *)
    
    def output(value:List[A]) = {
      val size =
        if(value.size < 256) Output.Literal.char(small) ++ Output.byte.unsigned(value.size)
        else Output.Literal.char(large) ++ int32.pickle(value.size)      
      size ++ (Output * pickler.pickle)(value)
    }
    Pickler(parser, output)
  }
  
  def length(small:Char, large:Char):Pickler[Array[Byte]] = {
    val parser =
      ((Parsers.Literal.char(small) ~> Parsers.byte.unsigned
      | Parsers.Literal.char(large) ~> Parsers.BigEndian.int32) >> (Parsers.byte *)) ^^ (_.toArray)
    
    def output(value:Array[Byte]) = {
      val length =
        if(value.length < 256) Output.Literal.char(small) ++ byte.unsigned.pickle(value.length)
        else Output.Literal.char(large) ++ int32.pickle(value.length)
      length ++ value
    }
    Pickler(parser, output) 
  }
}
