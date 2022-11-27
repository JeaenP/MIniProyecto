def simpson(limI: Int, limS: Int, f: Double => Double) = (limS - limI)*(((f(limI) + 4*(f((limS + limI)/ 2)) + f(limS)))/6)

def simpsonCompuesta(limI: Int, limS: Int, n: Int, f: Double => Double) = {
  val x = (j: Double) => (limI + (((limS - (limI * 1.0))/ n) * j))
  (((limS - limI * 1.0)/ n)/3)* ((1 to n/2).map(i => (f(x(2 * i - 2)) + 4 * f(x(2 * i - 1)) + f(x(2 * i))))).sum
}