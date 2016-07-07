package scalaz
package optics

import scalaz.typeclass._
import scalaz.data._
import Forget._


trait Lens[S, T, A, B] { self =>
  def stab[P[_, _]: Strong]: P[A, B] => P[S, T]

  def get(s: S): A = {
    val p = Strong[({ type l[a, b] = Forget[A, a, b]})#l]
    (stab[({ type l[a, b] = Forget[A, a, b]})#l])(p)(Forget[A, A, B](a => a)).forget(s)
  }
  
  def set(b: B, s: S): T = {
    val p = Strong[Function1]
    stab[Function1](p)((a: A) => b)(s)
  }

  def compose[C, D](lens: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
    override def stab[P[_,_]: Strong]: P[C, D] => P[S, T] = (pcd: P[C, D]) => {
      val pab = lens.stab(Strong[P])(pcd)
      self.stab(Strong[P])(pab)
    }
  }
}

object Lens {
  
  def apply[S, T, A, B](get: S => A, set: (B, S) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def stab[P[_, _]: Strong]: P[A, B] => P[S, T] = (pab: P[A, B]) => { 
      val t = Strong[P].first[A, B, S](pab)
      Profunctor[P].dimap(t)((s: S) => (get(s),s ))(t => set(t._1, t._2))
    }
  }
  case class User(name: String, age: Int)

}

