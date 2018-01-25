class rational(x:Int, y:Int) {
  require(y != 0, "denominator cannot be zero")
  //override def toString() = if (numer==denom) "1.0" else if (numer==0) "0.0" else numer/(if(0!=g) g else y) + "/" + denom/(if(0!=g) g else y)
  override def toString() = numer + "/" + denom
  private def gcd(a:Int, b:Int) :Int = if(0==b) a else gcd(b, a%b)
  private val g = gcd(numer,denom)

  def numer:Int = x
  def denom:Int = y


  def add(that:rational) = new rational(this.numer * that.denom + that.numer*this.denom, this.denom*that.denom )
  def neg: rational = new rational(-numer, denom)
  def sub(that:rational) = add(that.neg)
  def mul(that:rational) = new rational(this.numer*that.numer, this.denom*that.denom)

  def less(that:rational) = numer*that.denom < denom*that.numer
  def max(that:rational) = if(this.less(that)) that else this

  def < (that:rational) = less(that)
  def unary_- : rational = new rational(-numer, denom)
  def +(that:rational) = add(that)
  def - (that:rational) = this + -that
  def * (that:rational) = mul(that)
}

val a = new rational(1,2)
val b = new rational(1,2)
val aneg = -a
val bneg = b.neg

val c = a.add(b)
val d =a.neg
val e = a.sub(b)

val x = new rational(1,3)
val y = new rational(5,7)
val z = new rational(3,2)

x max a
x < a

x sub y mul z
x - y *z


x.sub(y).sub(z)
x-y-z

x add y mul z
x + (y * z)


val strange = new rational(1,0)


