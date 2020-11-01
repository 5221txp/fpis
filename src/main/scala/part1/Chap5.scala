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
}