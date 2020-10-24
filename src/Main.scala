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

  def zad4a(): String = {
    println("====== ZADANIE 4A ======")

    list.foldLeft("")((a, b) => a + b + ", ").dropRight(2)

  }

  def zad4b(): String = {
    println("====== ZADANIE 4B ======")

    list.foldRight("")((a, b) => a + ", " + b).dropRight(2)

  }

  def zad4c(): String = {
    println("====== ZADANIE 4C ======")


    list.filter(day => day.startsWith("P")).foldLeft(""){(b,a) => b + a + ", "}.dropRight(2)

  }

  def zad5() : Unit = {
    println("====== ZADANIE 5 ======")

    val cena_wyjsciowa = Map("czekolada"-> 4, "salami" -> 26, "sushi" -> 100, "nalewka" ->20)
    val cena_promocyjna = cena_wyjsciowa.transform((_, v) => v * 0.9)

    println("Cena wyjściowa: " + cena_wyjsciowa)
    println("Cena promocyjna: " + cena_promocyjna)
  }

  def zad6(t: (Char,String, Int)) : Unit ={
    println("====== ZADANIE 6 ======")

    println(t)
  }

  def zad7() : Unit =  {
    println("====== ZADANIE 7 ======")

    val cena_wyjsciowa = Map("czekolada"-> 4, "salami" -> 26, "sushi" -> 100, "nalewka" ->20)

    println("Szukane - sushi. Wynik: " + zad7_option(cena_wyjsciowa.get("sushi")))
    println("Szukane - sake. Wynik: " + zad7_option(cena_wyjsciowa.get("sake")))
  }

  def zad7_option(x: Option[Any]) = x match {
    case Some(a) =>"znaleziono: "+ a
    case None => "nie znaleziono szukanego klucza"
  }

  def zad8(inputList: List[Int]) : List[Int]= {
    println("====== ZADANIE 8 ======")

    @tailrec
    def ziobroRemoval(inputList: List[Int], outputList: List[Int]): List[Int] = inputList match{
      case Nil => outputList
      case head :: tail => {
        if(head == 0) ziobroRemoval(tail, outputList)
        else ziobroRemoval(tail, outputList.appended(head))
      }
    }
    ziobroRemoval(inputList, List.empty[Int])

  }

  def zad9(inputList : List[Int]): List[Int] = {
    println("====== ZADANIE 9 ======")

    inputList.map(x => x + 1)
  }


  def zad10(inputL : List[Double]) : List[Double] = {
    println("====== ZADANIE 10 ======")
    var l1 = -5
    var l2 = 12

    inputL.filter(_ >= l1)
      .filter(_ <= l2)
      .map(x => x.abs)
  }




  def main(args: Array[String]){
    println(zad1a())

    println(zad1b())

    println(zad1c())

    println(zad2a())

    println(zad2b())

    println(zad3())

    println(zad4a())

    println(zad4b())

    println(zad4c())

    zad5()

    var tuple = ('a', "Ulu mulu", 21)
    zad6(tuple)

    zad7()

    var listWithZeros = List(0,1,0,2,0,0,3,0,4,0,5,0,0,6,0,7,0,8,0,9,0,10)
    println(zad8(listWithZeros))

    var numList = List(20,36,1,3,99,0,4)
    println(zad9(numList))

    var lastList = List(-1000, -5, -5.2, -4.9, -7, 6,2,1,33,79,0,1.6,3.54,12, -3, -2.45, 8.4,12.1)
    println(zad10(lastList))
  }

}
