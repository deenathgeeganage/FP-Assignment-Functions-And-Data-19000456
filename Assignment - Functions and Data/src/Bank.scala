object Bank extends App {


  val bank : List[Account] = List( new Account("Deenath",5000), new Account("noobie",1000),new Account("nub",-500))

  val negativeacc = (l :List[Account])=> l.filter(l=>l.getbal()<0)

  val totalbal = (l:List[Account])=> l.map(l=>(l,l.getbal())).reduce((x,y)=>(x._1,x._2+y._2))

  val calinterest = (l:List[Account])=>l.map(a=>{
    if (a.getbal()> 0) a.deposit(a.getbal()*0.05) else a.withdraw(-a.getbal()*0.1)
  } )






  println("-------------------------------------")
  bank(0).transer(bank(1),1000)
  println("-------------------------------------")
  println(bank(0))
  println(bank(1))
  println("-------------------------------------")
  println("Accounts with Negative Balance\n"+negativeacc(bank))
  println("-------------------------------------")
  println("Total Balance of Accounts before Interest\n"+totalbal(bank)._2)
  println("-------------------------------------")
  println("Calculate Intrest\n")
  calinterest(bank)
  println("-------------------------------------")
  println("Total Balance of Accounts after Interest\n"+totalbal(bank)._2)








}


class Account(a:String,n:Double){
  val name:String = a
  var balance:Double = n

  def getbal(): Double ={
    balance
  }

  def setbal(x:Double): Unit ={
    this.balance = x
  }

  def deposit(n:Double): Any ={
    println("Depositing "+n+" to "+this.name)
    this.balance += n
  }

  def withdraw(n:Double):Any ={
    println("Withdrawing "+n+" from "+this.name)
    this.balance -= n
  }

  def transer(a:Account,x:Double): Any ={
    println("Transferring "+ x +" to "+ a.name +" from "+ this.name)
    this.balance -= x
    a.balance += x
  }

  override def toString: String = "Account holder : "+ name +"\n  || Account Balance : "+ balance
//  override def toString: String = name

}
