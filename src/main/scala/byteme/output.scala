package byteme

import java.nio.{ByteOrder, ByteBuffer}


trait Output { self =>
  def length:Int
  
  def toArray:Array[Byte] = {
    val array = Array.ofDim[Byte](length)
    write(array, 0)
    array
  }
  
  def write(array:Array[Byte], from:Int)
  
  def ++ (other:Output) = new Output{
    def length = self.length + other.length
    def write(array: Array[Byte], from: Int) {
      self.write(array, from)
      other.write(array, from + self.length)
    }
  }
}

object Output {
  val empty:Output = new Output{
    def length = 0
    def write(array: Array[Byte], from: Int) {}
  }
  
  def * [A](times:Int, f:A => Output):List[A] => Output =
    list => if(list.size == times) (Output * f)(list) else sys.error("requires input of size " + times + ", but got of size " + list.size) 
  
  def * [A](f:A => Output): List[A] => Output =
    list => list.foldLeft(Output.empty)((o, e) => o ++ f(e))

  implicit object byte extends (Byte => Output){
    def apply(b:Byte):Output = new Output{
      def length = 1
      override def toArray = Array(b)
      def write(array: Array[Byte], from: Int) {
        array(from) = b
      }
    }
    def unsigned(i:Int) = {
      if(i >= 0 && i < 256)
        byte(i.toByte)
      else
        throw new RuntimeException("unsigned byte must be in range 0 - 255 (was "+ i +")")
    }
  }
  
  implicit def array(b:Array[Byte]):Output = new Output{
    def length = b.length
    override def toArray = b
    def write(array: Array[Byte], from: Int) {
      System.arraycopy(b, 0, array, from, length)
    }
  }
  
  object Literal {
    implicit def int(i:Int) = byte(i.toByte)
    implicit def char(c:Char) = byte(c.toByte)    
  }
  
  abstract class Endian(order:ByteOrder) {

    private abstract class BufferOutput(val length:Int) extends Output{
      def put(buffer:ByteBuffer)
      def write(array: Array[Byte], from: Int) {
        put(ByteBuffer.wrap(array, from, length).order(order))
      }
    }
    
    def int16(s: Short):Output = new BufferOutput(2){
      def put(buffer: ByteBuffer) { buffer.putShort(s) }
    }
    def int32(i: Int):Output = new BufferOutput(4){
      def put(buffer: ByteBuffer) { buffer.putInt(i) }
    }
    def int64(l: Long):Output = new BufferOutput(8){
      def put(buffer: ByteBuffer) { buffer.putLong(l) }
    }
    def float(f: Float):Output = new BufferOutput(4){
      def put(buffer: ByteBuffer) { buffer.putFloat(f) }
    }
    def double(d: Double):Output = new BufferOutput(8){
      def put(buffer: ByteBuffer) { buffer.putDouble(d) }
    }
  }
  
  object LittleEndian extends Endian(ByteOrder.LITTLE_ENDIAN)
  object BigEndian extends Endian(ByteOrder.BIG_ENDIAN)  
}