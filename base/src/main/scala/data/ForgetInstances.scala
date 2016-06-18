package scalaz
package data
import typeclass.{Profunctor, Strong, Choice, ChoiceClass, StrongClass}

trait ForgetInstances { self =>
  implicit def choice[A]: Choice[Forget[A, ?, ?]] = new ChoiceClass[Forget[A, ?, ?]] {
   
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]

    def left[A, B, C](pab: P[A, B]): P[A \/ C, B \/ C] = ???

    def right[A, B, C](pab: P[A, B]): P[C \/ A, C \/ B] = ???
 
  }
}

