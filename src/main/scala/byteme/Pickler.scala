package byteme

object Pickler {
  def apply[A](unpickle:Parser[A], pickle:A => Output) = new Pickler[A](unpickle, a => Some(pickle(a)))
}

class Pickler[A](val unpickle:Parser[A], val tryPickle:A => Option[Output]) { self =>
  
  def pickle(value:A) = tryPickle(value).get  
  
  def ~ [B](rhs: => Pickler[B]):Pickler[A ~ B] = {
    lazy val other = rhs
    Pickler(unpickle ~ other.unpickle, { case a ~ b => self.pickle(a) ++ rhs.pickle(b) })
  }
  
  def * = Pickler(unpickle *, Output * pickle)
  
  def * (times:Int) = Pickler[List[A]](unpickle * times, Output * (times, pickle))

  def *[B](pickler: => Pickler[B])(implicit ev:A => Int, ev1:Int => A) = {
    lazy val b = pickler
    Pickler[List[B]](unpickle.map(ev) >> (pickler.unpickle *), l => pickle(l.size) ++ (Output * b.pickle)(l))
  }
  
  def bytes (implicit ev:A => Int, ev1:Int => A) = 
    Pickler[Array[Byte]](unpickle.map(ev) >> Parsers.bytes, l => pickle(l.length) ++ Output.array(l))

  def lengthExclusive[B](pickler: => Pickler[B])(implicit ev:A => Int, ev1:Int => A) = {
    // TODO, verify number of consumed bytes on unpickle
    lazy val b = pickler
    def pickle(value:B) = {
      val out = b.pickle(value)
      self.pickle(out.length) ++ out
    }
    Pickler(unpickle.map(ev) ~> pickler.unpickle, pickle)
  }
  
  def wrap[B](w:A => B)(u:B => A) = 
    new Pickler[B](unpickle.map(w), v => tryPickle(u(v)))
  
  def | [T >: A, B <: T](rhs: => Pickler[B])(implicit a:Picklers.Reify[A], b:Picklers.Reify[B]):Pickler[T] = {
    lazy val other = rhs
    val left:Parser[T] = unpickle
    lazy val right:Parser[T] = other.unpickle
    new Pickler[T](left | right, t => a.reify(t).flatMap(tryPickle) orElse b.reify(t).flatMap(other.tryPickle))
  }
   
  def <~ [B](const:Picklers.Constant[B]) =
    Pickler[A](unpickle <~ const.unpickle, a => pickle(a) ++ const.pickle(const.constant))
  
  def ^^ [B](w:Picklers.Wrap[A, B]):Pickler[B] = wrap(w.wrap)(w.unwrap)
  
  def takeWhile(f: A => Boolean) =
    Pickler[List[A]](unpickle.takeWhile(f), Output * pickle)
}

object Picklers {
  
  def bytes(n:Int):Pickler[Array[Byte]] =
    Pickler[Array[Byte]](Parsers.bytes(n), arr => (Output * (n, byte.pickle))(arr))  
  
  object byte extends Pickler[Byte](Parsers.byte, b => Some(Output.byte(b))) {
    override val unpickle = Parsers.byte
    val unsigned = FixedLength(1, Parsers.byte.unsigned, Output.byte.unsigned)
    
    def until(b:Byte) =
      Pickler[Array[Byte]](Parsers.byte.until(b), arr => Output.array(arr.takeWhile(_ != b)))
  }
  
  case class Constant[A](constant:A, underlying:Pickler[A]) extends Pickler[A](underlying.unpickle.constant(constant), v => if(v == constant) Some(underlying.pickle(v)) else None){ self =>
    def ~> [B] (rhs: => Pickler[B]) = (this ~ rhs).wrap{ case a ~ b => b }{b => new ~(constant, b) }    
    def ^^^ [B](b: => B) = Constant(b, wrap(_ => b)(_ => constant))
  }
  
  trait FixedLength { self:Pickler[Int] =>
    def length:Int
    
    def lengthInclusive[A](pickler:Pickler[A]) = {
      // todo, check that unpickler consumes the specified number of bytes
      def pickle(value:A) = {
        val out = pickler.pickle(value)
        self.pickle(out.length + length) ++ out
      }      
      Pickler(unpickle ~> pickler.unpickle, pickle)
    }
  }
  
  object FixedLength{
    def apply(l:Int, unpickle:Parser[Int], pickle:Int => Output) = new Pickler[Int](unpickle, a => Some(pickle(a))) with FixedLength {
      def length = l
    }
  }
  
  trait Reified[A] extends Pickler[A]{
    def reify[B >: A](b:B):Option[A]
  }
  
  object Literal {
    implicit def char(c:Char) = Constant(c.toByte, Picklers.byte)
    implicit def int(i:Int)   = Constant(i.toByte, Picklers.byte)
  }
  
  abstract class Endian(parser:Parsers.Endian, output:Output.Endian) {
    val int16  = Pickler(parser.int16, output.int16)
    val int32  = FixedLength(4, parser.int32, output.int32)
    val int64  = Pickler(parser.int64, output.int64)
    val float  = Pickler(parser.float, output.float)
    val double = Pickler(parser.double, output.double)
  }
  
  object BigEndian extends Endian(Parsers.BigEndian, Output.BigEndian)
  object LittleEndian extends Endian(Parsers.LittleEndian, Output.LittleEndian)
  
  trait Reify[A]{
    def reify(any:Any):Option[A]
    
    def unapply(any:Any) = reify(any)
  }
  
  object Reify extends InstanceOfReify {
    def apply[A](f:PartialFunction[Any, A]) = new Reify[A]{
      def reify(any: Any) = f.lift(any)
    }

    implicit val byte    = Reify{ case b:Byte    => b }
    implicit val boolean = Reify{ case b:Boolean => b }
    implicit val short   = Reify{ case s:Short   => s }
    implicit val int     = Reify{ case i:Int     => i }
    implicit val long    = Reify{ case l:Long    => l }
    implicit val float   = Reify{ case f:Float   => f }
    implicit val double  = Reify{ case d:Double  => d }
    implicit val char    = Reify{ case c:Char    => c }
    implicit val string  = Reify{ case s:String  => s }
    implicit def option[A : Reify] = {
      val ReifyA = implicitly[Reify[A]]
      Reify{
        case Some(ReifyA(a)) => Some(a)
        case None => None
      }
    }
    implicit def tuple2[A:Reify,B:Reify] = {
      val ReifyA = implicitly[Reify[A]]
      val ReifyB = implicitly[Reify[B]]
      Reify{
        case (ReifyA(a),ReifyB(b)) => (a, b)
      }                                      
    }
    implicit def tilde[A : Reify, B : Reify] = {
      val ReifyA = implicitly[Reify[A]]
      val ReifyB = implicitly[Reify[B]]
      Reify{ case ReifyA(a) ~ ReifyB(b) => new ~(a, b) }
    }
  }
  
  trait InstanceOfReify {
    implicit def instance[A <: AnyRef : Manifest]:Reify[A] = new Reify[A]{
      val man = implicitly[Manifest[A]]
      
      def reify(any: Any) = any match {
        case ref:AnyRef =>
          val result = man >:> ClassManifest.singleType(ref) 
          if (result) Some(ref.asInstanceOf[A]) else None
        case _ => None
      }
    }
  }
  
  case class Wrap[A, B](wrap:A => B, unwrap:B => A){
    def ^^ [C](next:Wrap[B, C]) = Wrap[A, C](wrap andThen next.wrap, next.unwrap andThen unwrap)
  }
  
  object strings {
    def apply(name:String) = Wrap[Array[Byte], String](b => new String(b, name), _.getBytes(name)) 
    val utf8 = apply("UTF-8")
  }  
}
