package scalaz
package data

import typeclass.{Profunctor, Strong, Choice, ChoiceClass, StrongClass, Monoid}
import data.Disjunction._

trait ForgetInstances { self =>
  implicit def choice[A](implicit M: Monoid[A]): Choice[Forget[A, ?, ?]] = new ChoiceClass[Forget[A, ?, ?]] {
   
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]

    override def left[B, C, D](pab: Forget[A, B, C]): Forget[A, B \/ D, C \/ D] = Forget(bd => bd.fold(l => pab.forget(l))(r => M.empty)) 
    //def right[B, C, D](pab: P[A, B, C]): P[C \/ A, C \/ B] = 
 
  }
  
  implicit def profunctor[A]: Profunctor[Forget[A, ?, ?]] = new Profunctor[Forget[A, ?, ?]] {
   
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]
  }
}

