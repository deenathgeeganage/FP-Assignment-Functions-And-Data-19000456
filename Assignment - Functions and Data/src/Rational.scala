object Rational extends App {
  val x = new Rational(3,4)
  println(x.neg)

  val y = new Rational(5,8)
  val z = new Rational(2,7)

  println(x-y-z)

}

class Rational(n:Int,d:Int){
  require(d>0,"Denominator of a rational number cannot be 0")
  def numer: Int = n/gcd(n,d)
  def denom: Int = d/gcd(n,d)



  //auxiliary function
  def this(n:Int) = this(n,1)

  private def gcd(a:Int,b:Int): Int ={
    if(b==0) if (a>0){
      a
    } else {
      -a
    }
    else gcd(b,a%b)
  }
  def add(r:Rational) = new Rational(this.numer*r.denom+this.denom*r.numer,this.denom*r.denom)
  def +(r:Rational) = new Rational(this.numer*r.denom+this.denom*r.numer,this.denom*r.denom)

  def -(r:Rational) = new Rational(this.numer*r.denom-this.denom*r.numer,this.denom*r.denom)

  override def toString: String = numer+"/"+denom

  def neg = new Rational(-this.numer,this.denom)
}
