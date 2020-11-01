package scala.part1

object Example {
  def check(): Unit = {
    var data = List("1", "2", "3")
    println(zipWith(data, data)(_ + _))
    println(traverse(data)((s) => Try(s.toInt)))
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(l3: List[A], l4: List[B], l5: List[C])(f1: (A, B) => C): List[C] = {
      (l3, l4) match {
        case (Nil, _) => l5
        case (_, Nil) => l5
        case (x :: xs, y :: ys) => loop(xs, ys, l5 :+ f(x, y))(f1)
      }
    }
    loop(l1, l2, List())(f)
    // (l1, l2) match {
    //   case (Nil, _) => Nil
    //   case (_, Nil) => Nil
    //   case (x :: xs, y :: ys) => f(x, y) :: zipWith(xs, ys)(f)
    // }
  }

  def Try[A](a: => A): Option[A] =
    try {
      Some(a)
    } catch { case e: Exception => None }

  def map2[A, B, C] (a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a match {
      case None => None
      case Some(a1) => b map (f(a1, _))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    @annotation.tailrec
    def loop(a1: List[A], b: List[B])(f1: A => Option[B]): Option[List[B]] = {
      a1 match {
        case Nil => Some(b)
        case x :: xs => f1(x) match {
          case None => None
          case Some(y) => loop(xs, b :+ y)(f1)
        }
      }
    }
    (loop(a, List())(f))

    // a match {
    //   case Nil => Some(List())
    //   case x :: xs => map2(f(x), traverse(xs)(f))(_ :: _)
    // }
  }

}