package byteme

sealed trait Result[+A]{
  def map[B](f:A => B):Result[B]
  def flatMapWithNext[B](f:A => Input => Result[B]):Result[B]
  def orElse[B >: A](b: => Result[B]):Result[B]
  def get:A
  def isSuccess:Boolean
}
case class Success[+A](value:A, next:Input) extends Result[A]{
  def map[B](f: (A) => B) = Success(f(value), next)
  def flatMapWithNext[B](f: A => Input => Result[B]) = f(value)(next)
  def orElse[B >: A](b: => Result[B]) = this
  def get = value
  def isSuccess = true
}
case class Failure(msg:String, input:Input) extends Result[Nothing]{
  def map[B](f: (Nothing) => B) = this
  def flatMapWithNext[B](f: (Nothing) => (Input) => Result[B]) = this
  def orElse[B >: Nothing](b: => Result[B]) = b
  def get = throw new NoSuchElementException(toString)
  def isSuccess = false
}
case class Error(msg:String, input:Input) extends Result[Nothing]{
  def map[B](f: (Nothing) => B) = this
  def flatMapWithNext[B](f: (Nothing) => (Input) => Result[B]) = this
  def orElse[B >: Nothing](b: => Result[B]) = this
  def get = throw new NoSuchElementException(toString)
  def isSuccess = false
}