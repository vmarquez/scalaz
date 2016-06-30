package scalaz
package optics

import scalaz.typeclass._
import scalaz.data._
import Forget._
/*

trait Lens[S, T, A, B] { self =>
  def stab[P[_, _]: Strong]: P[A, B] => P[S, T]

  def get(s: S): A = {
    val p = Strong[({ type l[a, b] = Forget[A, a, b]})#l]
    (stab[({ type l[a, b] = Forget[A, a, b]})#l])(p)(Forget[A, A, B](a => a)).forget(s)
  }
  def set(b: B, s: S): T = ???
}

object MakeLens {
  //T == A?  
  def mapply[S, T, A, B](get: S => A, set: (B, S) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def stab[P[_, _]: Strong]: P[A, B] => P[S, T] = (pab: P[A, B]) => { 
      val tupled = Strong[P].first[A, B, S](pab)
      Profunctor[P].dimap(tupled)((s: S) => (get(s),s ))(t => set(t._1, t._2))
    }
  }
  //my Test 
  case class User(name: String, age: Int)
 
  //need a lense that takes a user, sets a new user, and returns. (is it polymorphic?)

}
*/

