package byteme

import java.nio.{ByteOrder, ByteBuffer}
import Parsers._

trait Endian {
  def int16: Parser[Short]
  def int32: Parser[Int]
  def int64: Parser[Long]
  def float: Parser[Float]
  def double: Parser[Double]
}

object LittleEndian extends Endian {
  private def buffer(l:List[Byte]) = ByteBuffer.wrap(l.toArray).order(ByteOrder.LITTLE_ENDIAN)
  val int16  = (byte * 2) ^^ { buffer(_).asShortBuffer().get(0) }
  val int32  = (byte * 4) ^^ { buffer(_).asIntBuffer().get(0) }
  val int64  = (byte * 8) ^^ { buffer(_).asLongBuffer().get(0) }
  val float  = (byte * 4) ^^ { buffer(_).asFloatBuffer().get(0) }
  val double = (byte * 8) ^^ { buffer(_).asDoubleBuffer().get(0) }
}

object BigEndian extends Endian {
  private def buffer(l:List[Byte]) = ByteBuffer.wrap(l.toArray).order(ByteOrder.BIG_ENDIAN)
  val int16  = (byte * 2) ^^ { buffer(_).asShortBuffer().get(0) }
  val int32  = (byte * 4) ^^ { buffer(_).asIntBuffer().get(0) }
  val int64  = (byte * 8) ^^ { buffer(_).asLongBuffer().get(0) }
  val float  = (byte * 4) ^^ { buffer(_).asFloatBuffer().get(0) }
  val double = (byte * 8) ^^ { buffer(_).asDoubleBuffer().get(0) }
}
