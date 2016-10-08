package scalaz
package data

import typeclass.{Profunctor, Strong, Choice, ChoiceClass, StrongClass, Monoid}
import data.Disjunction._

trait ForgetInstances { self =>
  implicit def strong[A]: Strong[Forget[A, ?, ?]] = new StrongClass[Forget[A, ?, ?]] {
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]

    override def first[B, C, D](fbc: Forget[A, B, C]): Forget[A, (B, D), (C, D)] = 
      Forget[A, (B, D), (C, D)](bd => fbc.forget(bd._1))  
   
    override def second[B, C, D](fbc: Forget[A, B, C]): Forget[A, (D, B), (D, C)] =
      Forget[A, (D, B), (D, C)](db => fbc.forget(db._2))
  }
  
  
  implicit def choice[A](implicit M: Monoid[A]): Choice[Forget[A, ?, ?]] = new ChoiceClass[Forget[A, ?, ?]] {
     
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]

    override def leftchoice[B, C, D](pab: Forget[A, B, C]): Forget[A,  B \/ D, C \/ D] =
      Forget( {
        case \/-(d) => M.empty 
        case -\/(b) => pab.forget(b)
      })

    override def rightchoice[B, C, D](pab: Forget[A, B, C]): Forget[A, D \/ B, D \/ C] =
      Forget( {
        case \/-(b) => pab.forget(b)
        case -\/(c) => M.empty 
      })
  }
}

