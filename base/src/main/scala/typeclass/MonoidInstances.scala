package scalaz
package typeclass

trait MonoidInstances {
  
  implicit def listMonoidInstance[A]: Monoid[List[A]] = new MonoidClass[List[A]] {
      def append(f1: List[A], f2: => List[A]) = f1 ++ f2
      override def empty: List[A] = List[A]()
    }
}
