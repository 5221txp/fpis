trait RNG {
  def nextInt: (Int, RNG)

  def nonNegativeInt: (Int, RNG) = {
    val (n, nextRNG) = nextInt
    (math.abs(n%Int.MaxValue)+1, nextRNG)
  }
}
case class SimpleRNG(seed: Long) extends RNG
{
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Chap6 {

  type Rand[+A] = RNG => (A, RNG)

  def double(rng: RNG): (Double, RNG) = {
    val (n, nextRNG) = rng.nextInt
    (math.abs(n%Int.MaxValue).toDouble/Int.MaxValue.toDouble, nextRNG)
  }

  def doubleWithMap: Rand[Double] =
    map(_.nextInt)(n => math.abs(n%Int.MaxValue).toDouble/Int.MaxValue.toDouble)

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng1) = rng.nextInt
    val (d, rng2) = double(rng1)
    ((n, d), rng2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (n, rng2) = rng1.nextInt
    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(c: Int)(rng1: RNG)(l: List[Int]): (List[Int], RNG) = c match {
      case 0 => (l, rng1)
      case _ => {
        val (n, nextRNG) = rng1.nextInt
        loop(c-1)(nextRNG)(n :: l)
      }
    }
    loop(count)(rng)(List())
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](l: List[Rand[A]]): Rand[List[A]] = rng => {
    @annotation.tailrec
    def loop(rng1: RNG, l1: List[Rand[A]], l2: List[A]): (List[A], RNG) = {
      l1 match {
        case Nil => (l2, rng1)
        case x :: xs => {
          val (a, rng2) = x(rng1)
          loop(rng2, xs, a :: l2)
        }
      }
    }
    loop(rng, l, List())
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng1) = f(rng)
    g(a)(rng1)
  }

  def nonNegativeIntLessThan(rng: RNG)(n: Int): (Int, RNG) = {
    val r = flatMap(rng1 => rng1.nonNegativeInt)((a: Int) => (rng2: RNG) => (a % n, rng2))
    r(rng)
  }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => rng1 => (f(a), rng1))

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(both(ra, rb))(a => rng1 => (f(a._1, a._2), rng1))

  type State[S,+A] = S => (A,S)

  def unitState[S,A](a: A): State[S,A] = s => (a, s)

  def mapState[S,A,B](sa: State[S,A])(f: A => B): State[S,B] = s => {
    val (a, s1) = sa(s)
    (f(a), s1)
  }

  def map2State[S,A,B,C](sa: State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] = s => {
    val (a, s1) = sa(s)
    val (b, s2) = sb(s1)
    (f(a, b), s2)
  }

  def flatMapState[S,A,B](f: State[S,A])(g: A => State[S,B]): State[S,B] = s => {
    val (a, s1) = f(s)
    g(a)(s1)
  }
}
