package scalaz
package data

import Maybe._
import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance

/**
 * Safe, invariant alternative to stdlib `List`. Most methods on `List` have a sensible equivalent 
 * here, either on the `IList` interface itself or via typeclass instances (which are the same as
 * those defined for stdlib `List`). All methods are total and stack-safe.
 */
sealed abstract class IList[A] {

  // Operations, in alphabetic order

  def ++(as: IList[A]): IList[A] = 
    concat(as)

  def ++:(as: IList[A]): IList[A] = 
    concat(as)

  def +:(a: A): IList[A] = 
    ::(a)

  def /:[B](b: B)(f: (B, A) => B): B = 
    foldLeft(b)(f)

  def :+(a: A): IList[A] = 
    concat(IList(a))  

  def ::(a: A): IList[A] = 
    ICons(a, this) 

  def :::(as: IList[A]): IList[A] = 
    ++:(as)

  def :\[B](b: B)(f: (A, B) => B): B = 
    foldRight(b)(f)

  def collect[B](pf: PartialFunction[A,B]): IList[B] = 
    flatMap(a => IList.fromMaybe(pf.lift(a).asMaybe))

  def collectFirst[B](pf: PartialFunction[A,B]): Maybe[B] = 
    find(a => pf.lift(a).isDefined).map(pf)

  def concat(as: IList[A]): IList[A] = 
    foldRight(as)(_ :: _)

  // no contains; use Foldable#element

  def count(f: A => Boolean): Int = 
    foldLeft(0)((n, a) => if (f(a)) n + 1 else n)

  def drop(n: Int): IList[A] = {
    @tailrec def drop0(as: IList[A], n: Int): IList[A] =
      if (n < 1) as else as match {
        case INil() => INil()
        case ICons(_, t) => drop0(t, n - 1)
      }
    drop0(this, n)
  }

  def dropRight(n: Int): IList[A] = 
    reverse.drop(n).reverse

  def dropRightWhile(f: A => Boolean): IList[A] = 
    reverse.dropWhile(f).reverse

  def dropWhile(f: A => Boolean): IList[A] = {
    @tailrec def dropWhile0(as: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if (f(h)) => dropWhile0(t)
        case a => a
      }
    dropWhile0(this)
  }

  // no exists; use Foldable#any

  def filter(f: A => Boolean): IList[A] = 
    foldRight(IList.empty[A])((a, as) => if (f(a)) a :: as else as)

  def filterNot(f: A => Boolean): IList[A] = 
    filter(a => !f(a))

  def find(f: A => Boolean): Maybe[A] = {
    @tailrec def find0(as: IList[A])(f: A => Boolean): Maybe[A] =
      as match {
        case INil() => Empty[A] 
        case ICons(a, as) => if (f(a)) Just(a) else find0(as)(f)
      }
    find0(this)(f)
  }

  def flatMap[B](f: A => IList[B]): IList[B] = 
    foldRight(IList.empty[B])(f(_) ++ _)

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def foldLeft0(as: IList[A])(b: B)(f: (B, A) => B): B =
      as match {
        case INil() => b
        case ICons(a, as) => foldLeft0(as)(f(b, a))(f)
      }
    foldLeft0(this)(b)(f)
  }

  def foldRight[B](b: B)(f: (A, B) => B): B = 
    reverse.foldLeft(b)((b, a) => f(a, b))

  // no forall; use Foldable#all
  // no foreach; use Each#each if you dare


  def headMaybe: Maybe[A] = 
    uncons(Empty[A], (h, _) => Just(h))

  def indexWhere(f: A => Boolean): Maybe[Int] = {
    @tailrec def indexWhere0(i: Int, as: IList[A]): Maybe[Int] =
      as match {
        case INil() => Empty[Int]
        case ICons(h, t) => if (f(h)) Just(i) else indexWhere0(i + 1, t)
      }
    indexWhere0(0, this)
  }

  def initOption: Maybe[IList[A]] = 
    reverse.tailMaybe.map(_.reverse)

  def inits: IList[IList[A]] = 
    reverse.tails.map(_.reverse)

  def intersperse(a: A): IList[A] = {
    @tailrec def intersperse0(accum: IList[A], rest: IList[A]): IList[A] = rest match {
      case INil() => accum
      case ICons(x, INil()) => x :: accum
      case ICons(h, t) => intersperse0(a :: h :: accum, t)
    }
    intersperse0(INil(), this).reverse
  }

  def isEmpty: Boolean = 
    uncons(true, (_, _) => false)

  def lastIndexWhere(f: A => Boolean): Maybe[Int] =
    reverse.indexWhere(f).map((length - 1) - _)

  def lastMaybe: Maybe[A] = 
    reverse.headMaybe

  def length: Int = 
    foldLeft(0)((n, _) => n + 1)

  def map[B](f: A => B): IList[B] = 
    foldRight(IList.empty[B])(f(_) :: _)

  // private helper for mapAccumLeft/Right below
  private[this] def mapAccum[B, C](as: IList[A])(c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    as.foldLeft((c, IList.empty[B])) 
      { case ((c, bs), a) => {
          val t = f(c, a)
          (t._1, t._2 :: bs)
        } 
      }

  /** All of the `B`s, in order, and the final `C` acquired by a stateful left fold over `as`. */
  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, IList[B]) = {
    val t = mapAccum(this)(c, f)  
    (t._1, t._2.reverse) 
  }

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired by a stateful right fold over `as`. */
  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    mapAccum(reverse)(c, f)

  // no min/max; use Foldable#minimum, maximum, etc.

  def nonEmpty: Boolean = 
    !isEmpty

  def padTo(n: Int, a: A): IList[A] = {
    @tailrec def padTo0(n: Int, init: IList[A], tail: IList[A]): IList[A] =
      if (n < 1) init.reverse ++ tail else tail match {
        case INil() => padTo0(n - 1, a :: init, INil())
        case ICons(h, t) => padTo0(n - 1, h :: init, t)
      }
    padTo0(n, INil(), this)
  }

  def partition(f: A => Boolean): (IList[A], IList[A]) = {
    val t = foldLeft((IList.empty[A], IList.empty[A])) {
      case ((ts, fs), a) => if (f(a)) (a :: ts, fs) else (ts, a :: fs)
    }
    (t._1.reverse, t._2.reverse)
  }

  def patch(from: Int, patch: IList[A], replaced: Int): IList[A] = {
    val (init, tail) = splitAt(from)
    init ++ patch ++ (tail drop replaced)
  }

  def prefixLength(f: A => Boolean): Int = {
    @tailrec def prefixLength0(n: Int, as: IList[A]): Int =
      as match {
        case ICons(h, t) if (f(h)) => prefixLength0(n + 1, t)
        case _ => n
      }
    prefixLength0(0, this)
  }

  // no product, use Foldable#fold

  def reduceLeftMaybe(f: (A, A) => A): Maybe[A] = 
    uncons(Empty[A], (h, t) => Just(t.foldLeft(h)(f)))

  def reduceRightMaybe(f: (A, A) => A): Maybe[A] = 
    reverse.reduceLeftMaybe((a, b) => f(b, a)) 

  def reverse: IList[A] = 
    foldLeft(IList.empty[A])((as, a) => a :: as)

  def reverseMap[B](f: A => B): IList[B] =
    foldLeft(IList.empty[B])((bs, a) => f(a) :: bs)

  def reverse_:::(as: IList[A]): IList[A] =
    as.foldLeft(this)((as, a) => a :: as)

  def scanLeft[B](z: B)(f: (B, A) => B): IList[B] =
    reverse.scanRight(z)((a, b) => f(b, a)).reverse

  def scanRight[B](z: B)(f: (A, B) => B): IList[B] =
    foldRight((IList(z), z)) { case (a, (bs, b)) => 
      val b0 = f(a, b)
      (b0 :: bs, b0)
    }._1

  def slice(from: Int, until: Int): IList[A] =
    drop(from).take((until max 0)- (from max 0))

  def span(f: A => Boolean): (IList[A], IList[A]) = {
    @tailrec def span0(as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      as match {
        case INil() => (accum.reverse, INil())
        case ICons(h, t) => if (f(h)) span0(t, h :: accum) else (accum.reverse, as)
      }
    span0(this, INil())
  }

  def splitAt(n: Int): (IList[A], IList[A]) = {
    @tailrec def splitAt0(n: Int, as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      if (n < 1) (accum.reverse, as) else as match {
        case INil() => (accum.reverse, INil())
        case ICons(h, t) => splitAt0(n - 1, t, h :: accum)
      }
    splitAt0(n, this, INil())
  }
  // no sum, use Foldable#fold

  def tails: IList[IList[A]] = {
    @tailrec def inits0(as: IList[A], accum: IList[IList[A]]): IList[IList[A]] =
      as match {
        case INil() => (as :: accum).reverse
        case ICons(_, t) => inits0(t, as :: accum)
      }
    inits0(this, INil())
  }

  def tailMaybe: Maybe[IList[A]] = 
    uncons(Empty[IList[A]], (_, t) => Just(t))

  def take(n: Int): IList[A] = {
    @tailrec def take0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      if (n < 1) accum else as match {
        case INil() => accum
        case ICons(h, t) => take0(n - 1, t, h :: accum)
      }
    take0(n, this, INil()).reverse
  }

  def takeRight(n: Int): IList[A] = 
    reverse.take(n).reverse

  def takeRightWhile(f: A => Boolean): IList[A] = 
    reverse.takeWhile(f).reverse

  def takeWhile(f: A => Boolean): IList[A] = {
    @tailrec def takeWhile0(as: IList[A], accum: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if f(h) => takeWhile0(t, h :: accum)
        case _ => accum
      }
    takeWhile0(this, INil()).reverse
  }

    def toList: List[A] = 
    foldRight(Nil : List[A])(_ :: _)

  def toVector: Vector[A] = 
    foldRight(Vector[A]())(_ +: _)

  def uncons[B](n: => B, c: (A, IList[A]) => B): B =
    this match {
      case INil() => n
      case ICons(h, t) => c(h, t)
    }

  /** Unlike stdlib's version, this is total and simply ignores indices that are out of range */
  def updated(index: Int, a: A): IList[A] = {
    @tailrec def updated0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      (n, as) match {
        case (0, ICons(h, t)) => accum.reverse ++ ICons(a, t)
        case (n, ICons(h, t)) => updated0(n - 1, t, h :: accum)
        case _ => accum.reverse
      }
    updated0(index, this, INil())
  }

  // many other zip variants; see Traverse#zip*

  def zip[B](b: => IList[B]): IList[(A, B)] = {
    @tailrec def zaccum(a: => IList[A], b: => IList[B], accum: IList[(A,B)]): IList[(A, B)] =
      (a, b) match {
        case (ICons(a, as), ICons(b, bs)) => zaccum(as, bs, (a, b) :: accum)
        case _ => accum
      }
    zaccum(this, b, INil()).reverse
  }

  def zipWithIndex: IList[(A, Int)] = 
    zip(IList(0 until length : _*))

}

// In order to get exhaustiveness checking and a sane unapply in both 2.9 and 2.10 it seems
// that we need to use bare case classes. Sorry. Suggestions welcome.
case class INil[A]() extends IList[A]
case class ICons[A](head: A, tail: IList[A]) extends IList[A]

object IList {

  def apply[A](as: A*): IList[A] = 
    as.foldRight(empty[A])(ICons(_, _))

  def empty[A](): IList[A] =
    INil[A]()

  def fromList[A](as: List[A]): IList[A] = 
    as.foldRight(empty[A])(ICons(_, _))
  
  def fromMaybe[A](a: Maybe[A]): IList[A] = 
    a.fold(a => IList(a), IList.empty[A])

  def fill[A](n: Int)(a: A): IList[A] =
    INil().padTo(n, a)
}
