sealed abstract trait Option2[+A] {
  def map[B](f: A => B): Option2[B] = this match {
    case None2 => None2
    case Some2(a) => Some2(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None2 => default
    case Some2(a) => a
  }

  def flatMap[B](f: A => Option2[B]): Option2[B] = map(f) getOrElse None2

  def orElse[B >: A](ob: => Option2[B]): Option2[B] = map(Some2(_)) getOrElse ob

  def filter(f: A => Boolean): Option2[A] = flatMap(a => if(f(a)) Some2(a) else None2)
}
case class Some2[+A](get: A) extends Option2[A]
case object None2 extends Option2[Nothing]


sealed trait Either2[+E, +A] {
  def map[B](f: A => B): Either2[E, B] = this match {
    case Left2(e) => Left2(e)
    case Right2(a) => Right2(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either2[EE, B]): Either2[EE, B] = this match {
    case Left2(e) => Left2(e)
    case Right2(a) => f(a) match {
      case Left2(ee) => Left2(ee)
      case Right2(b) => Right2(b)
    }
  }

  def orElse[EE >: E,B >: A](b: => Either2[EE, B]): Either2[EE, B] = this match {
    case Left2(e) => b
    case Right2(a) => Right2(a)
  }

  def map2[EE >: E, B, C](b: Either2[EE, B])(f: (A, B) => C): Either2[EE, C] = (this, b) match {
    case (Left2(e), _) => Left2(e)
    case (_, Left2(e)) => Left2(e)
    case (Right2(a), Right2(b1)) => Right2(f(a, b1))
  }
}
case class Left2[+E](value: E) extends Either2[E, Nothing]
case class Right2[+A](value: A) extends Either2[Nothing, A]


object Chap4 {
  def variance(xs: Seq[Double]): Option2[Double] = {
    val mean: Seq[Double] => Option2[Double] = (s: Seq[Double]) => {
      val total = s.foldLeft(0: Double)(_ + _)
      if(total != 0) Some2(total/s.length) else None2
    }
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option2[A], b: Option2[B])(f: (A, B) => C): Option2[C] = {
    // a.map(a1 => ((b1: B) => f(a1, b1))).flatMap(f1 => b.map(f1))
    (a, b) match {
      case (None2, _) => None2
      case (_, None2) => None2
      case (Some2(a1), Some2(b1)) => Some2(f(a1, b1))
    }
  }

  def sequence[A](l: List[Option2[A]]): Option2[List[A]] = {
    l.foldRight[Option2[List[A]]](Some2(List()))((a, l1) => map2(a, l1)(_ :: _))}

  def sequence2[A](l: List[Option2[A]]): Option2[List[A]] = traverse(l)(x => x)

  def traverse[A, B](l: List[A])(f: A => Option2[B]): Option2[List[B]] = l match {
    case Nil => Some2(Nil)
    case x :: xs => f(x).flatMap(b => traverse(xs)(f).map(b :: _))
  }

  def sequenceEither[E, A](l: List[Either2[E, A]]): Either2[E, List[A]] = traverseEither(l)(x => x)

  def traverseEither[E, A, B](l: List[A])(f: A => Either2[E, B]): Either2[E, List[B]] = l match {
    case Nil => Right2(Nil)
    case x :: xs => f(x).flatMap(b => traverseEither(xs)(f).map(b :: _))
  }
}