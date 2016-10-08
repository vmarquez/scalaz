package scalaz
package optics

import scalaz.data._
import scalaz.typeclass._
import scalaz.data.Disjunction._
import scalaz.data.Maybe._
import scalaz.typeclass.Monoid._

trait Prism[S, T, A, B] {
  def stab[P[_, _]: Choice]: P[A, B] => P[S, T]

  def getMaybe(s: S): Maybe[A] = {
    val la = (stab[Forget[List[A], ?, ?]])(Choice[Forget[List[A], ?, ?]])(Forget[List[A], A, B](a => List(a))).forget(s)
    la.headOption.fold[Maybe[A]](empty[A])( s => Just(s))
  }

  def get(b: B): T =
    (stab[RConst](Choice[RConst])(RConst[A, B](b))).rconst
}

object Prism {
  def apply[S, T, A, B](sa: S => \/[T, A], bt: B => T): Prism[S, T, A, B] = new Prism[S, T, A, B] {
    override def stab[P[_, _]](implicit P: Choice[P]): P[A, B] => P[S, T] = (pab: P[A, B]) =>
      P.profunctor.dimap(P.rightchoice[A, B, T](pab))(sa)(e => e.fold(identity)(bt))
  }
}

object test{
  def maybeprism[A] = Prism[Maybe[A], Unit, A, Unit](s => s match {
    case Just(a) => \/-(a)
    case _ => -\/(()) 
  }, _ => -\/(()))
}


