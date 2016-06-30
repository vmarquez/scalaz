package scalaz
package optics

import scalaz.typeclass._
import scalaz.data._
import Forget._

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
  /*
  def compose[C, D](abcd: Prism[A, B, C, D]: Prism[S, T, C, D] = new Prism[S, T, C, D] {
    override def stab[P[_, _]: Profunctor]: P[
  }*/
  
}

trait Lens[S, T, A, B] { self =>
  def stab[P[_, _]: Profunctor]: P[A, B] => P[S, T]

  def get(s: S): A = ???
  def set(b: B, s: S): T = ???
}
//trait SimpleLens[A, B] {
  //def stab[P[_, _]: Profunctor]: P[A, B] => 
//}

object MakeLens {
  //T == A?  
  def apply[S, T, A, B](get: S => A, set: (b, s) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def stab[P[_, _]: Strong]: P[A, B] => P[S, T] = {
      (pab: P[A, B]) => Strong[P].dimap[(A, S), (B, S), S, T](Strong[P].first[A, B, S])(s => (get, s))(bs => set(bs._1, bs._2)
    }

  }

  //my Test 
  case class User(name: String, age: Int)
 
  //need a lense that takes a user, sets a new user, and returns. (is it polymorphic?)

}



object Iso {
  def apply[S, T, A, B](sa: S => A, bt: B => T): Iso[S, T, A, B] = new Iso[S, T, A, B] {
    override def stab[P[_, _]](implicit P: Profunctor[P]): P[A, B] => P[S, T] = Profunctor[P].dimap(_)(sa)(bt) 
  }
}
