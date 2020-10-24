import scala.annotation.tailrec

object Main {

  val list = List("Poniedzialek","Wtorek","Sroda","Czwartek","Piatek","Sobota","Niedziela")

  def zad1a(): String = {
    println("====== ZADANIE 1A ======")
    var outputStr = ""
    for (i <- list) {
      outputStr += i+", "
    }
    outputStr.dropRight(2)
  }

  def zad1b() : String = {
    println("====== ZADANIE 1B ======")
    var outputStr = ""
    for (i <- list) {
      if(i.startsWith("P")){
        outputStr += i + ", "
      }
    }
    outputStr.dropRight(2)
  }

  def zad1c() : String = {
    println("====== ZADANIE 1C ======")
    var outputStr = ""
    var i = 0
    while(i < list.length){
      outputStr += list(i) + ", "
      i += 1
    }
    outputStr.dropRight(2)
  }

  def zad2a(): String ={
    println("====== ZADANIE 2A ======")

    zad2arek(list, "", list.length).dropRight(2)
  }


  def zad2arek(x : List[String], str : String, n: Int):String= {
    if(n == 0)
      str
    else
      zad2arek(x.tail, str + x.head + ", ", n-1)

  }

  def zad2b() : String ={
    println("====== ZADANIE 2B ======")

    zad2arek(list.reverse, "", list.length).dropRight(2)
  }

  def zad3() : String ={
    println("====== ZADANIE 3 ======")
    @tailrec
    def zad3rek(x : List[String], str : String):String = x match {
      case Nil => str.dropRight(2)
      case head :: tail => zad3rek(tail, str + head + ", ")
    }
    zad3rek(list,"")
  }




  def main(args: Array[String]){
    println(zad1a())

    println(zad1b())

    println(zad1c())

    println(zad2a())

    println(zad2b())

    println(zad3())
  }

}
