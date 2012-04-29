package byteme

import Parsers.{success, commit}

case class ~[+A, +B](_1:A, _2:B)

object Parser {
  def apply[A](f:Input => Result[A]):Parser[A] = new Parser[A]{
    def apply(input:Input) = f(input)
  }
}

trait Parser[+A] extends (Input => Result[A]){ self =>
  def * :Parser[List[A]] = takeWhile(_ => true)

  def * (times:Int):Parser[List[A]] =
    if(times <= 0) success(Nil)
    else for{
      a <- this
      b <- this * (times - 1)
    } yield a :: b

  def ~ [B] (b: => Parser[B]):Parser[A ~ B] = {
    lazy val other = b
    for{
      aa <- this
      bb <- other
    } yield new ~(aa, bb)
  }
  
  def ~> [B] (b: => Parser[B]):Parser[B] = {
    lazy val other = b
    for{
      _ <- this
      bb <- other
    } yield bb
  }
  
  def ~>! [B] (b: => Parser[B]):Parser[B] = 
    this ~> commit(b)

  def <~ [B] (b: => Parser[B]):Parser[A] = {
    lazy val other = b
    for {
      aa <- this
      _ <- other
    } yield aa
  }

  def | [B >: A](b: => Parser[B]):Parser[B] = {
    lazy val other = b
    Parser{ input => apply(input) orElse other(input) }
  }

  def ^^^ [B](b:B):Parser[B] = ^^ { _ => b }

  def ^^ [B](f:A => B):Parser[B] = map(f)

  def >> [B](f:A => Parser[B]):Parser[B] = flatMap(f)

  def flatMap[B](f:A => Parser[B]) = Parser{ apply(_).flatMapWithNext(f) }

  def map[B](f:A => B) = Parser{ apply(_).map(f) }

  def filter(f: A => Boolean) = where(f, "filter")

  def withFilter(f: A => Boolean) = filter(f)

  def takeWhile(f:A => Boolean):Parser[List[A]] =
    (for{
      a <- this if f(a)
      b <- takeWhile(f)
    } yield a :: b) | success(Nil)

  def where(f:A => Boolean, msg:String) = Parser{ input =>
    apply(input) match {
      case Success(a, _) if !f(a) => Failure(msg, input)
      case n => n
    }
  }
}