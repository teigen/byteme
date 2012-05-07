package byteme

import java.nio.{ByteOrder, ByteBuffer}

object Parsers {
  object byte extends Parser[Byte]{
    def apply(input: Input) =
      if(input.atEnd) Failure("eof", input)
      else Success(input.first, input.rest)
    
    def unsigned = map(_ & 0xFF)
  }

  implicit def literalByte(b:Byte) = byte.where(_ == b, "expected " + b)
  
  object Literal {
    implicit def int(i:Int)   = byte.where(_ == i.toByte, "expected " + i)
    implicit def char(c:Char) = byte.where(_ == c.toByte, "expected " + c)
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
    private def buffer(l:List[Byte]) = ByteBuffer.wrap(l.toArray).order(order)

    val int16  = (byte * 2) ^^ { buffer(_).getShort(0) }
    val int32  = (byte * 4) ^^ { buffer(_).getInt(0) }
    val int64  = (byte * 8) ^^ { buffer(_).getLong(0) }
    val float  = (byte * 4) ^^ { buffer(_).getFloat(0) }
    val double = (byte * 8) ^^ { buffer(_).getDouble(0) }
  }

  object LittleEndian extends Endian(ByteOrder.LITTLE_ENDIAN)
  object BigEndian extends Endian(ByteOrder.BIG_ENDIAN)
}