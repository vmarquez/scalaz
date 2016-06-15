package scalaz
package optics

import scalaz.data._
import scalaz.typeclass._
import scalaz.data.Disjunction._

trait Prism[S, T, A, B] {
  def stab[P[_, _]: Choice]: P[A, B] => P[S, T]
}

object Prism {
  def apply[S, T, A, B](sa: S => \/[T, A], bt: B => T): Prism[S, T, A, B] = new Prism[S, T, A, B] {
    override def stab[P[_, _]](implicit P: Choice[P]): P[A, B] => P[S, T] = (pab: P[A, B]) =>
      P.profunctor.dimap(P.right[A, B, T](pab))(sa)(e => e.fold(identity)(bt))
  }
}



