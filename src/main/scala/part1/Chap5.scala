sealed trait Stream2[+A] {
  def toList: List[A] = this match {
    case Empty2 => Nil
    case Cons2(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream2[A] = this match {
    case Cons2(h, t) if n > 1 => Stream2.cons(h(), t().take(n - 1))
    case Cons2(h, _) if n == 1 => Stream2.cons(h(), Stream2.empty)
    case _ => Stream2.empty
  }

  @annotation.tailrec
  final def drop(n: Int): Stream2[A] = this match {
    case Cons2(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream2[A] = this match {
    case Empty2 => Empty2
    case Cons2(h, t) => if (p(h())) Cons2(h, () => t().takeWhile(p)) else Empty2
  }

  def takeWhileWithFoldRight(p: A => Boolean): Stream2[A] =
    this.foldRight(Empty2: Stream2[A])((a, b) => if (p(a)) Cons2(() => a, () => b) else Empty2)

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty2 => true
    case Cons2(h, t) => if (p(h())) t().forAll(p) else false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons2(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def headOption: Option[A] = this match {
    case Empty2 => None
    case Cons2(h, t) => Some(h())
  }

  def headOptionWithFoldRight: Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream2[B] =
    this.foldRight(Empty2: Stream2[B])((a, b) => Cons2(() => f(a), () => b))

  def filter(p: A => Boolean): Stream2[A] =
    this.foldRight(Empty2: Stream2[A])((a, b) => if (p(a)) Cons2(() => a, () => b) else b)

  def append[B>:A](s: => Stream2[B]): Stream2[B] =
    this.foldRight(s)((a, b) => Cons2(() => a, () => b))

  def flatMap[B](f: A => Stream2[B]): Stream2[B] =
    this.foldRight(Empty2: Stream2[B])((a, b) => f(a) append b)
}
case object Empty2 extends Stream2[Nothing]
case class Cons2[+A](h: () => A, t: () => Stream2[A]) extends Stream2[A]

object Stream2 {
  def cons[A](hd: => A, tl: => Stream2[A]): Stream2[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons2(() => head, () => tail)
  }

  def empty[A]: Stream2[A] = Empty2

  def apply[A](as: A*): Stream2[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream2[A] = Cons2(() => a, () => constant(a))

  def from(n: Int): Stream2[Int] = Cons2(() => n, () => from(n+1))

  def fibs: Stream2[Int] = {
    def gen(n1: Int, n2: Int): Stream2[Int] = Cons2(() => n1, () => gen(n2, n1 + n2))
    gen(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream2[A] = f(z) match {
    case None => Empty2
    case Some((a, s)) => Cons2(() => a, () => unfold(s)(f))
  }

  def constantWithUnfold[A](a: A): Stream2[A] =
    unfold(0)(s => Some(a, s))

  def fromWithUnfold(n: Int): Stream2[Int] =
    unfold(n)(s => Some(s, s+1))

  def fibsWithUnfold: Stream2[Int] =
    unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2)))

  def mapWithUnfold[A, B](s: Stream2[A])(f: A => B): Stream2[B] =
    unfold(s)(s => s match {
      case Empty2 => None
      case Cons2(h, t) => Some(f(h()), t())
    })
  // Continue with 5.13
}