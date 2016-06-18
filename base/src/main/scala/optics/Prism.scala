package scalaz
package optics

import scalaz.data._
import scalaz.typeclass._
import scalaz.data.Disjunction._
import scalaz.data.Maybe._

trait Prism[S, T, A, B] {
  def stab[P[_, _]: Choice]: P[A, B] => P[S, T]

  def getMaybe(s: S): Maybe[A] = {
    val p = Choice[({ type l[a, b] = Forget[\/[T, A], a, b]})#l]
    val x = (stab[({ type l[a, b] = Forget[\/[T, A], a, b]})#l])(p)(Forget[\/[T, A], A, B](a => \/-(a))).forget(s)
    //x.fold(a => Just(a))(Empty[A])   
    x.fold[Maybe[A]](l => Empty[A])(a => Just(a))
  }
}

object Prism {
  def apply[S, T, A, B](sa: S => \/[T, A], bt: B => T): Prism[S, T, A, B] = new Prism[S, T, A, B] {
    override def stab[P[_, _]](implicit P: Choice[P]): P[A, B] => P[S, T] = (pab: P[A, B]) =>
      P.profunctor.dimap(P.right[A, B, T](pab))(sa)(e => e.fold(identity)(bt))
  }
}

object test{
  def maybeprism[A] = Prism[Maybe[A], Unit, A, Unit](s => s match {
    case Just(a) => \/-(a)
    case _ => -\/(()) 
  }, _ => -\/(()))
}


