package byteme

object Pickler {
  def apply[A](u:Input => Result[A], p:A => Output):Pickler[A] = new Pickler[A]{
    def pickle(value: A) = p(value)
    def unpickle(input: Input) = u(input)
  }
}

trait Pickler[A] { self =>
  
  def pickle(value:A):Output
  def unpickle(input:Input):Result[A]
  
  def tryPickle(value:A):Option[Output] = Some(pickle(value))
  
  def ~ [B](rhs: => Pickler[B]):Pickler[A ~ B] = {
    lazy val other = rhs
    val parse = for{
      a <- Parser(self.unpickle)
      b <- Parser(other.unpickle)
    } yield new ~(a, b)
    Pickler[A ~ B](parse, { case a ~ b => self.pickle(a) ++ rhs.pickle(b) })
  }
  
  def * = Pickler(Parser(unpickle) *, Output * pickle)
  
  def * (times:Int) = 
    Pickler[List[A]](Parser(unpickle) * times, Output * (times, pickle))
  
  def wrap[B](w:A => B)(u:B => A) = 
    Pickler[B](unpickle(_).map(w), v => pickle(u(v)))
  
  def | [T >: A, B <: T](rhs: => Pickler[B])(implicit a:Picklers.Reify[A], b:Picklers.Reify[B]):Pickler[T] = new Pickler[T]{
    lazy val other = rhs
    
    override def tryPickle(t:T) = a.reify(t).flatMap(self.tryPickle).orElse(b.reify(t).flatMap(other.tryPickle))
    
    def pickle(t:T) = tryPickle(t).get
    
    def unpickle(input:Input) = {
      val a:Parser[T] = Parser(self.unpickle)
      val b:Parser[T] = Parser(other.unpickle)
      (a | b)(input)
    }
  }
  
  def <~ [B](const:Picklers.Constant[B]) =
    Pickler[A](Parser(unpickle) <~ Parser(const.unpickle), a => pickle(a) ++ const.pickle(const.constant))
  
  def ^^ [B](w:Picklers.Wrap[A, B]):Pickler[B] = wrap(w.wrap)(w.unwrap)
  
  def takeWhile(f: A => Boolean) =
    Pickler[List[A]](Parser(unpickle).takeWhile(f), Output * pickle)
}

object Picklers {
  
  object byte extends Pickler[Byte]{
    def pickle(a: Byte) = Output.byte(a)
    def unpickle(input: Input) = Parsers.byte(input)
    
    def unsigned = Pickler(Parsers.byte.unsigned, Output.byte.unsigned)      
  }
  
  case class Constant[A](constant:A, underlying:Pickler[A]) extends Pickler[A]{ self =>
    
    override def tryPickle(a:A) = if(a == constant) Some(underlying.pickle(a)) else None
    def pickle(value: A) = tryPickle(value) getOrElse sys.error("expected " + constant +", but got " + value)
    def unpickle(input: Input) = Parser(underlying.unpickle).where(_ == constant, "expected " + constant)(input)

    def ~> [B] (rhs:Pickler[B]) = new Pickler[B]{
      lazy val other = rhs
      def pickle(value: B) = self.pickle(constant) ++ other.pickle(value)
      def unpickle(input: Input) = self.unpickle(input).flatMapWithNext(_ => other.unpickle)
    }
    
    def ^^^ [B](b: => B) = Constant(b, self.wrap(_ => b)(_ => self.constant))
  }
  
  case class FixedLength(length:Int, underlying:Pickler[Int]) extends Pickler[Int]{ self =>
    override def tryPickle(value:Int) = underlying.tryPickle(value)
    def pickle(value: Int) = underlying.pickle(value)
    def unpickle(input: Input) = underlying.unpickle(input)
    
    def lengthInclusive[A](pickler:Pickler[A]) = {
      // todo, check that unpickler consumes the specified number of bytes
      val unpickle = Parser(underlying.unpickle) ~> Parser(pickler.unpickle)
      def pickle(value:A) = {
        val out = pickler.pickle(value)
        underlying.pickle(out.length + length) ++ out
      }      
      Pickler(unpickle, pickle)
    }
    
    def lengthExclusive[A](pickler:Pickler[A]) = {
      val unpickle = Parser(underlying.unpickle) ~> Parser(pickler.unpickle)
      def pickle(value:A) = {
        val out = pickler.pickle(value)
        underlying.pickle(out.length) ++ out
      }
      Pickler(unpickle, pickle)
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
    val int32  = FixedLength(4, Pickler(parser.int32, output.int32))
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
  
  object collections {
    def array[A : ClassManifest] = Wrap[List[A], Array[A]](_.toArray, _.toList)
  }
}
