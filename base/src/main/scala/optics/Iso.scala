package scalaz
package optics

import scalaz.typeclass._
import scalaz.data._

trait Iso[S, T, A, B] { self =>

  def stab[P[_, _]: Profunctor]: P[A, B] => P[S, T]

  def get(s: S): A = {
    val p = Profunctor[({ type l[a, b] = Forget[A, a, b]})#l]
    (stab[({ type l[a, b] = Forget[A, a, b]})#l])(p)(Forget[A, A, B](a => a)).forget(s)
  }

  def rget(b: B): T = {
    val p = implicitly[Profunctor[RConst]]
    ((stab[RConst])(p)(RConst[A, B](b))).rconst
  }

  def flip: Iso[B, A, T, S] = Iso(rget _, get _)    

  def compose[C, D](abcd: Iso[A, B, C, D]): Iso[S, T, C, D] = new Iso[S, T, C, D] { 
    override def stab[P[_, _]: Profunctor]: P[C, D] => P[S, T] = abcd.stab[P](Profunctor[P]) andThen self.stab[P](Profunctor[P])
  }
}

object Iso {
  def apply[S, T, A, B](sa: S => A, bt: B => T): Iso[S, T, A, B] = new Iso[S, T, A, B] {
    override def stab[P[_, _]](implicit P: Profunctor[P]): P[A, B] => P[S, T] = Profunctor[P].dimap(_)(sa)(bt) 
  }
}
