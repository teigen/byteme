package byteme

trait Input{
  def first:Byte
  def rest:Input
  def atEnd:Boolean
  def length:Int
  def view(begin:Int, stop:Int):Input
  def toArray:Array[Byte]
}

object Input{
  def apply(array:Array[Byte]) = ArrayInputView(array, 0, array.length)
  
  implicit def byteArrayIsInput(array:Array[Byte]) = apply(array)
}

case class ArrayInputView(array:Array[Byte], start:Int, end:Int) extends Input {
  if(!(start >= 0))
    throw new IndexOutOfBoundsException("start("+start+") >= 0")
  if(!(start <= end))
    throw new IndexOutOfBoundsException("start("+start+") <= end("+end+")")
  if(!(end <= array.length))
    throw new IndexOutOfBoundsException("end("+end+") <= array.length ("+array.length+")")

  def first = if(start < end) array(start) else throw new IndexOutOfBoundsException("calling .first on a view (" + start + " -> " + end +")")
  def rest = copy(start = start + 1)
  def atEnd = start == end
  def length = end - start
  def view(begin:Int, stop:Int) = copy(start = begin + start, end = start + stop)

  def toArray = {
    val a = Array.ofDim[Byte](end - start)
    System.arraycopy(array, start, a, 0, end - start)
    a
  }
}