import scala.annotation.tailrec

object Main {

  val list = List("Poniedzialek","Wtorek","Sroda","Czwartek","Piatek","Sobota","Niedziela")
  val cena_wyjsciowa_map = Map("czekolada"-> 4, "salami" -> 26, "sushi" -> 100, "nalewka" ->20)

  def zad1a(): String = {
    println("\n\t====== ZADANIE 1A ======")
    var outputStr = ""
    for (i <- list) {
      outputStr += i+", "
    }
    outputStr.dropRight(2)
  }

  def zad1b() : String = {
    println("\t====== ZADANIE 1B ======")
    var outputStr = ""
    for (i <- list) {
      if(i.startsWith("P")){
        outputStr += i + ", "
      }
    }
    outputStr.dropRight(2)
  }

  def zad1c() : String = {
    println("\t====== ZADANIE 1C ======")
    var outputStr = ""
    var i = 0
    while(i < list.length){
      outputStr += list(i) + ", "
      i += 1
    }
    outputStr.dropRight(2)
  }

  def zad2a(): String ={
    println("\n\t====== ZADANIE 2A ======")

    zad2arek(list, "", list.length).dropRight(2)
  }


  def zad2arek(x : List[String], str : String, n: Int):String= {
    if(n == 0)
      str
    else
      zad2arek(x.tail, str + x.head + ", ", n-1)

  }

  def zad2b() : String ={
    println("\t====== ZADANIE 2B ======")

    zad2arek(list.reverse, "", list.length).dropRight(2)
  }

  def zad3() : String ={

    @tailrec
    def zad3rek(x : List[String], str : String):String = x match {
      case Nil => str.dropRight(2)
      case head :: tail => zad3rek(tail, str + head + ", ")
    }
    zad3rek(list,"")
  }

  def zad4a(): String = {
    println("\n\t====== ZADANIE 4A ======")

    list.foldLeft("")((a, b) => a + b + ", ").dropRight(2)

  }

  def zad4b(): String = {
    println("\n\t====== ZADANIE 4B ======")

    list.foldRight("")((a, b) => a + ", " + b).dropRight(2)

  }

  def zad4c(): String = {
    println("\n\t====== ZADANIE 4C ======")

    list.filter(day => day.startsWith("P")).foldLeft(""){(b,a) => b + a + ", "}.dropRight(2)

  }

  def zad5() : Unit = {

    val cena_promocyjna = cena_wyjsciowa_map.transform((_, v) => v * 0.9)

    println("\nCena wyjściowa: " + cena_wyjsciowa_map)
    println("Cena promocyjna: " + cena_promocyjna)
  }

  def zad6(t: (Char,String, Int)) : Unit ={
    println("\n====== ZADANIE 6 ======")
    println("\n" + t)
  }

  def zad7() : Unit =  {
    println("\n====== ZADANIE 7 ======")

    println("\nMapa do przeszukania: " + cena_wyjsciowa_map)

    println("Szukane - sushi. Wynik: " + zad7_option(cena_wyjsciowa_map.get("sushi")))
    println("Szukane - sake. Wynik: " + zad7_option(cena_wyjsciowa_map.get("sake")))
  }

  def zad7_option(x: Option[Any]): String = x match {
    case Some(a) =>"znaleziono: "+ a
    case None => "nie znaleziono szukanego klucza"
  }

  def zad8(inputList: List[Int]) : List[Int]= {

    @tailrec
    def ziobroRemoval(inputList: List[Int], outputList: List[Int]): List[Int] = inputList match{
      case Nil => outputList
      case head :: tail =>
        if(head == 0) ziobroRemoval(tail, outputList)
        else ziobroRemoval(tail, outputList.appended(head))
    }
    ziobroRemoval(inputList, List.empty[Int])

  }

  def zad9(inputList : List[Int]): List[Int] = {
    inputList.map(x => x + 1)
  }


  def zad10(inputL : List[Double]) : List[Double] = {
    val l1 = -5
    val l2 = 12

    inputL.filter(_ >= l1)
      .filter(_ <= l2)
      .map(x => x.abs)
  }




  def main(args: Array[String]){
    println("\n====== ZADANIE 1 ======")
    println("\t"+ zad1a())

    println("\t"+zad1b())

    println("\t"+zad1c())

    println("\n====== ZADANIE 2 ======")
    println("\t" + zad2a())

    println("\t" + zad2b())

    println("\n====== ZADANIE 3 ======")
    println("\n" + zad3())

    println("\n====== ZADANIE 4 ======")
    println("\t" + zad4a())

    println("\t" + zad4b())

    println("\t" + zad4c())

    println("\n====== ZADANIE 5 ======")
    zad5()

    val tuple = ('a', "Ulu mulu", 21)
    zad6(tuple)

    zad7()

    println("\n====== ZADANIE 8 ======")
    val listWithZeros = List(0,1,0,2,0,0,3,0,4,0,5,0,0,6,0,7,0,8,0,9,0,10)
    println("\nLista wejściowa: " + listWithZeros)
    println("Lista wynikowa: " + zad8(listWithZeros))

    println("\n====== ZADANIE 9 ======")
    val numList = List(20,36,1,3,99,0,4)
    println("\nLista wejściowa: " + numList)
    println("Lista wynikowa: " + zad9(numList))

    println("\n====== ZADANIE 10 ======")
    val lastList = List(-1000, -5, -5.2, -4.9, -7, 6,2,1,33,79,0,1.6,3.54,12, -3, -2.45, 8.4,12.1)
    println("\nLista wejściowa: " + lastList)
    println("Lista wynikowa: " + zad10(lastList))
  }

}
