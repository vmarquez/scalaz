package scalaz
package typeclass

trait MonadInstances {
  implicit val option: Monad[Option] = new MonadClass[Option] {
    override def ap[A, B](oa: Option[A])(f: Option[A => B]): Option[B] = oa.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
    override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)
    override def pure[A](a: A): Option[A] = Option(a)
  }

  implicit val list: Monad[List] = new MonadClass[List] {
    override def ap[A, B](xs: List[A])(f: List[A => B]): List[B] = xs.flatMap(a => f.map(_(a)))
    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs.flatMap(f)
    override def map[A, B](xs: List[A])(f: A => B): List[B] = xs.map(f)
    override def pure[A](a: A): List[A] = List(a)
  }

  implicit def function1[A]: Monad[Function1[A, ?]] = new MonadClass[Function1[A, ?]] {
    override def ap[B, C](fa: A => B)(f: Function1[A, Function1[B, C]]): Function1[A, C] = (a: A) => f(a)(fa(a))
    override def map[B, C](fa: Function1[A, B])(f: B => C): Function[A, C] = (a: A) => f(fa(a))
    override def flatMap[B, C](fa: Function1[A, B])(f: B => Function1[A, C]): Function1[A, C] = (a: A) => f(fa(a))(a)
    override def pure[B](b: B): Function1[A, B] = (a: A) => b
  }
}
