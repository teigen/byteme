package byteme

case class ~[+A, +B](_1:A, _2:B){
  def ~[C](c:C) = new ~(this, c)
}