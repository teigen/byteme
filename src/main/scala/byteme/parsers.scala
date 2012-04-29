package byteme

object Parsers {
  object byte extends Parser[Byte]{
    def apply(input: Input) =
      if(input.atEnd) Failure("eof", input)
      else Success(input.first, input.rest)
    
    def unsigned = map(_ & 0xFF)
  }

  implicit def literalByte(b:Byte) = byte.where(_ == b, "expected " + b)
  
  object Literal {
    implicit def int(i:Int)   = byte.where(_ == i.toByte, "expected " + i.toByte)
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
}