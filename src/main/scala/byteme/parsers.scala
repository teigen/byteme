package byteme

import java.nio.{ByteOrder, ByteBuffer}

object Parsers {
  def bytes(n:Int):Parser[Array[Byte]] =
    Parser{ input =>
      if(n <= input.length) {
        val (value, next) = input.take(n)
        Success(value, next)
      } else Failure("eof (cannot consume " + n + " bytes)", input)
    }
  
  object byte extends Parser[Byte]{
    def apply(input: Input) =
      if(input.atEnd) Failure("eof", input)
      else Success(input.first, input.rest)
    
    def unsigned = map(_ & 0xFF)    
    
    def until(b:Byte):Parser[Array[Byte]] =
      Parser{ input =>
        val index = input.indexOf(b)
        if(index != -1){
          val (value, next) = input.take(index)
          Success(value, next)
        } else Failure("eof (cannot find '" + b + "')", input)
      }
  }

  implicit def literalByte(b:Byte) = byte.constant(b)
  
  object Literal {
    implicit def int(i:Int)   = byte.where(_ == i.toByte, "expected " + i)
    implicit def char(c:Char) = byte.where(_ == c.toByte, "expected '" + c + "'")
  }

  def fail(msg:String):Parser[Nothing] = Parser{ input => Failure(msg, input) }
  def success[A](value:A) = Parser{ input => Success(value, input) }
  
  def commit[A](parser:Parser[A]) = Parser{ in =>
    parser(in) match {
      case s@Success(_, _) => s
      case e@Error(_, _)   => e
      case Failure(msg, input) => Error(msg, input)
    }
  }

  abstract class Endian(order:ByteOrder) {
    private def buffer(l:Array[Byte]) = ByteBuffer.wrap(l).order(order)

    val int16  = bytes(2) ^^ { buffer(_).getShort(0) }
    val int32  = bytes(4) ^^ { buffer(_).getInt(0) }
    val int64  = bytes(8) ^^ { buffer(_).getLong(0) }
    val float  = bytes(4) ^^ { buffer(_).getFloat(0) }
    val double = bytes(8) ^^ { buffer(_).getDouble(0) }
  }

  object LittleEndian extends Endian(ByteOrder.LITTLE_ENDIAN)
  object BigEndian extends Endian(ByteOrder.BIG_ENDIAN)
}