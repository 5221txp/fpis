object Chap3List {
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: xs => xs
  }

  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => List(a)
    case x :: xs => a :: xs
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(l1: List[A], n1: Int): List[A] = n1 match {
      case 0 => l1
      case _ => loop(tail(l1), n1-1)
    }
    if(n > l.length) List()
    else loop(l, n)
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(l1: List[A]): List[A] = l1 match {
      case Nil => Nil
      case x :: xs => if (f(x)) loop(xs) else l1
    }
    loop(l)
  }

  def init[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l1: List[A], l2: List[A]): List[A] = l1 match {
      case Nil => Nil
      case x :: Nil => l2
      case x :: xs => loop(xs, l2 :+ x)
    }
    loop(l, List())
  }


  def foldRight[A,B](l: List[A], b: B)(f: (A, B) => B): B = l match {
    case Nil => b
    case x :: xs => f(x, foldRight(xs, b)(f))
  }

  def foldRight2[A,B](l: List[A], b: B)(f: (A, B) => B): B = {
    val r = (f: (A, B) => B) => (b: B, a: A) => f(a, b)
    foldLeft(foldLeft(l, Nil: List[A])((a,b) => b :: a), b)(r(f))
  }

  def lengthWithFoldRight[A](l: List[A]): Int = foldRight(l, 0)((_, y) => y + 1)

  def foldLeft[A,B](l: List[A], b: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l1: List[A], b1: B): B = l1 match {
      case Nil => b1
      case x :: xs => loop(xs, f(b1, x))
    }
    loop(l, b)
  }

  def appendFoldRight[A](l: List[A], a: A): List[A] = foldRight2(l, List(a))(_ :: _)

  def concat[A](l: List[List[A]]): List[A] = foldRight2(l, List(): List[A])(foldRight2(_, _)(_ :: _))

  def mapIncrease(l: List[Int]): List[Int] = {
    // @annotation.tailrec
    // def loop(l1: List[Int], l2: List[Int]): List[Int] = l1 match {
    //   case Nil => l2
    //   case x :: xs => loop(xs, x + 1 :: l2)
    // }
    // loop(l, List())
    foldRight2(l, List(): List[Int])(_ + 1 :: _)
  }

  def doublesToStrings(l: List[Double]): List[String] =
    foldRight2(l, List(): List[String])(_.toString :: _)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight2(l, List(): List[B])((x, y) => f(x) :: y)


  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight2(l, List(): List[A])((x, y) => if (f(x)) x :: y else y)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)((x) => if(f(x)) List(x) else List())

  def zipInt(l1: List[Int], l2: List[Int]): List[Int] = {
    @annotation.tailrec
    def loop(l3: List[Int], l4: List[Int], l5: List[Int]): List[Int] = (l3, l4) match {
      case(Nil, _) => l5
      case(_, Nil) => l5
      case(x :: xs, y :: ys) => loop(xs, ys, l5 :+ (x + y))
    }
    loop(l1, l2, List())
  }


  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    @annotation.tailrec
    def loop(l3: List[A], l4: List[B], l5: List[C]): List[C] = (l3, l4) match {
      case(Nil, _) => l5
      case(_, Nil) => l5
      case(x :: xs, y :: ys) => loop(xs, ys, l5 :+ f(x, y))
    }
    loop(l1, l2, List())
  }

  def hasSubSequence[A](l: List[A], s: List[A]): Boolean = {
    @annotation.tailrec
    def check(l1: List[A], s1: List[A]): Boolean =
      if(s1.length > l1.length) false
      else l1 match {
        case Nil => true
        case x ::xs => foldLeft(zipWith(l1, s1)(_ == _), true)(_ && _) || check(xs, s1)
      }
    check(l, s)
  }

}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Chap3Tree {
  def size[A](t: Tree[A]): Int = {
    def loop(t1: Tree[A], count: Int): Int = t1 match {
      case Leaf(_) => count + 1
      case Branch(l, r) => count + 1 + loop(l, 0) + loop(r,0)
    }
    loop(t, 0)
  }

  def maxInt(t: Tree[Int]): Int = {
    def loop(t1: Tree[Int], m: Int): Int = t1 match {
      case Leaf(i) =>  i max m
      case Branch(l, r) => loop(l, m) max loop(r, m)
    }
    loop(t, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A], b: B)(f: (A, B) => B): B = t match {
    case Leaf(a) => f(a, b)
    case Branch(l, r) => fold(r, fold(l, b)(f))(f)
  }

  def fold2[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold2(l)(f)(g), fold2(r)(f)(g))
  }

  def sizeWithFold[A](t: Tree[A]): Int = fold2(t)(_ => 1)(1 + _ + _)

  def depthWithFold[A](t: Tree[A]): Int = fold2(t)(_ => 1)((x: Int, y: Int) => 1 + (x max y))
}