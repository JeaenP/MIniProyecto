
def simpson(limI: Int, limS: Int, f: Double => Double) =
  (limS - limI)*(((f(limI) + 4*(f((limS + limI)/ 2)) + f(limS)))/6)

def simpsonCompuesta(limI: Int, limS: Int, n: Int, f: Double => Double) = {
  val x = (j: Double) => (limI + (((limS - (limI * 1.0))/ n) * j))
  (((limS - limI * 1.0)/ n)/3)* ((1 to n/2).map(i => (f(x(2 * i - 2)) + 4 * f(x(2 * i - 1)) + f(x(2 * i))))).sum
}

def simpsonExtendida(limI: Int, limS: Int, n: Int, f: Double => Double) = {
  val h = ((limS - limI * 1.0) / n)
  (h / 3) * (f(limI) + 4 * ((1 to n - 1 by 2).map(i => (f(limI + i * h)))).sum + 2 * ((2 to n - 2 by 2).map(i => (f(limI + i * h)))).sum + f(limS))
}

import math.*

@main def app: Unit =
  val funcion1 = (x: Double) => -(x * x)  + 8 * x - 12
  val funcion2 = (x: Double) => 3 * (x * x)
  val funcion3 = (x: Double) => x + 2 * ( x * x) - ( x * x * x) + 5 * ( x * x * x * x)
  val funcion4 = (x: Double) => (2 * x + 1)/( (x * x) + x)
  val funcion5 = (x: Double) => exp(x)
  val funcion6 = (x: Double) => (1)/math.sqrt(x - 1)
  val funcion7 = (x: Double) => 1/(1 + (x * x))

  println ("Funcion 1")
  println (simpson (3, 5, funcion1) )
  println (simpsonCompuesta (3, 5, 20, funcion1) )
  println (simpsonExtendida (3, 5, 20, funcion1) )

  println("Funcion 2")
  println(simpson(0, 2, funcion2))
  println(simpsonCompuesta(0, 2, 20, funcion2))
  println(simpsonExtendida(0, 2, 20, funcion2))

  println("Funcion 3")
  println(simpson(-1, 1, funcion3))
  println(simpsonCompuesta(-1, 1, 20, funcion3))
  println(simpsonExtendida(-1, 1, 20, funcion3))

  println("Funcion 4")
  println(simpson(1, 2, funcion4))
  println(simpsonCompuesta(1, 2, 20, funcion4))
  println(simpsonExtendida(1, 2, 20, funcion4))

  println("Funcion 5")
  println(simpson(0, 1, funcion5))
  println(simpsonCompuesta(0, 1, 20, funcion5))
  println(simpsonExtendida(0, 1, 20, funcion5))

  println("Funcion 6")
  println(simpson(2, 3, funcion6))
  println(simpsonCompuesta(2, 3, 20, funcion6))
  println(simpsonExtendida(2, 3, 20, funcion6))

  println("Funcion 7")
  println(simpson(0, 1, funcion7))
  println(simpsonCompuesta(0, 1, 20, funcion7))
  println(simpsonExtendida(0, 1, 20, funcion7))


