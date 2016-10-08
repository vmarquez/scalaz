package scalaz
package typeclass

trait StrongInstances { instances =>
  implicit val function: Strong[Function] = new StrongClass[Function] {
    override def lmap[A, B, C](ab: A => B)(ca: C => A): C => B = c => ab(ca(c))
    
    override def rmap[A, B, C](ab: A => B)(bc: B => C): A => C = a => bc(ab(a))
    
    override def dimap[A, B, C, D](ab: A => B)(ca: C => A)(bd: B => D): C => D = c => bd(ab(ca(c)))

    override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = _ match {
      case (a, c) => (pab(a), c)
    }

    override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = _ match {
      case (c, a) => (c, pab(a))
    }
  }
}
