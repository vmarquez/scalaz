package scalaz
package optics

import scalaz.data._
import scalaz.typeclass._
import scalaz.data.Disjunction._

trait EPrism[S, A, I] { self =>

  def stab[P[_, _]: Profunctor]: P[I \/ A, A] => P[I \/ S, S]
  
  def get(s: S): \/[I, A] = {
    val p = Profunctor[({ type l[a, b] = Forget[I \/ A, a, b]})#l]
    (stab[({ type l[a, b] = Forget[I \/ A, a, b]})#l])(p)(Forget[I \/ A, I \/ A, A](a => a)).forget(Disjunction.right(s))
  }

  def rget(a: A): S = {
    val p = implicitly[Profunctor[RConst]]
    ((stab[RConst])(p)(RConst[I \/ A, A](a))).rconst
  }

  def compose[B](ab: Iso[A, A, B, B]): EPrism[S, B, I] = new EPrism[S, B, I] { 
    override def stab[P[_,_]: Profunctor]: P[I \/ B, B] => P[I \/ S, S] = ab.stab[P](Profunctor[P]) andThen self.stab(Profunctor[P])
  }
}

  
object EPrism {
  def apply[S, A, I](sa: S => \/[I, A], as: A => S): EPrism[S, A, I] = new EPrism[S, A, I] {
    override def stab[P[_,_]](implicit P: Profunctor[P]): P[\/[I, A], A] => P[I \/ S, S] = Profunctor[P].dimap(_)((e: \/[I, S]) => e.flatMap(sa))(as)
  }

    //TODO: move to ISO 
    def toEPrism[S, A, I](i: Iso[S, A]): EPrism[S, A, I] = EPrism((i.get _).map(_.right), i.rget _)
    
    def compose[S, A, B, I](sa: EPrism[S, A, I])(ab: Iso[A, A, B, B]): EPrism[S, B, I] = new EPrism[S, B, I] { 
      override def stab[P[_,_]: Profunctor]: P[I \/ B, B] => P[I \/ S, S] = ab.stab[P](Profunctor[P]) andThen sa.stab(Profunctor[P])
    }
}

