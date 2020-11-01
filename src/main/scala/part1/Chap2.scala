object Chap2 {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n1: Int, n2: Int, nth: Int): Int = nth match {
      case 0 => n2
      case 1 => n1
      case _ => loop(n2, n1 + n2, nth - 1)
    }
    loop(0, 1, n)
  }

  def isSorted[A](arr: Array[A])(f: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = n match {
      case 0 => true
      case _ => f(arr(n-1), arr(n)) && loop(n-1)
    }
    loop(arr.length-1)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}