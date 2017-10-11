package scalaz
package data

import scalaz.typeclass.Compose
import scalaz.data.Disjunction._
import scalaz.typeclass.Bind._

abstract class FreeSemiArrow[P[_, _], A, B] {
  import FreeSemiArrow._ 
  
  //def evaluateSingle[P[_,_], A, B](pab: FreeSemiArrow[P, A, B])(implicit P: Profunctor[P]): (P[A, Any], Option[FreeSemiArrow[P, Any, B]]) = {
  //  
  //  this match {
  //    case ArrComposed(paz, -\/(pzb)) => rec(paz, pzb)
  //    case Lifted(pab) => (pab, None)
  //  }
  //}

  //def go[Z](paz: FreeSemiArrow[P, Z, A]): (P[
  //def rec[Z](paz: FreeSemiArrow[P, A, Z], pzb: FreeSemiArrow[P, Z, B]): (P[A, Z], FreeSemiArrow[P, Z, B]) =
  //  pab match {
  //    case ArrComposed(Lifted(pab), more) => ??? //(pab, more) //more comopsed with the passed in accumulator?
  //    case ArrComposed(ArrComposed(faz, -\/(eff)), more) =>  rec(faz, ArrComposed(eff, more))
  //    case ArrComopsed(ArrComposed(faz, \/-(f)), more) => rec(faz.rmap(f), more)
  //  }
 
  def compose[Z](that: FreeSemiArrow[P, Z, A]): FreeSemiArrow[P, Z, B] = {
      val x = -\/[FreeSemiArrow[P, A, B], Function1[A, B]](this) 
      val y = ArrComposed[P, Z, A, B](that, x)
      y 
  }

  def andThen[C](fbc: FreeSemiArrow[P, B, C]): FreeSemiArrow[P, A, C] =
    fbc compose this
} 

object FreeSemiArrow {
  import typeclass.{Profunctor, StrongClass, Strong } //, Category, StrongClass, CategoryClass}
  import typeclass.Strong._
  import typeclass.Profunctor._
  import data.Disjunction._
  import data.Disjunction.Syntax._
  import typeclass.Monad._
  
  def lift[P[_, _], A, B](pab: P[A, B]): FreeSemiArrow[P, A, B] =
    Lifted(pab) 
  
  case class Lifted[P[_, _], A, B](fab: P[A, B]) extends FreeSemiArrow[P, A, B]
  
  case class ArrComposed[P[_, _], A, B, C](fab: FreeSemiArrow[P, A, B], fbc: FreeSemiArrow[P, B, C] \/ Function1[B, C]) extends FreeSemiArrow[P, A, C]

  //def evaluateSingle[P[_,_], A, B](pab: FreeSemiArrow[P, A, B])(implicit P: Profunctor[P]): (P[A, Any], Option[FreeSemiArrow[P, Any, B]]) = {
  //  
  //  this match {
  //    case ArrComposed(paz, -\/(pzb)) => rec(paz, pzb)
  //    case Lifted(pab) => (pab, None)
  //  }
  //}

  //def rec[A, Z, B](paz: FreeSemiArrow[P, A, Z], pzb: FreeSemiArrow[P, Z, B]): (P[A, Z], FreeSemiArrow[P, Z, B]) =
  //  pab match {
  //    case ArrComposed(Lifted(pab), more) => ??? //(pab, more) //more comopsed with the passed in accumulator?
  //    case ArrComposed(ArrComposed(faz, -\/(eff)), more) =>  rec(faz, ArrComposed(eff, more))
  //    case ArrComopsed(ArrComposed(faz, \/-(f)), more) => rec(faz.rmap(f), more)
  //  }
 
  import scalaz.typeclass.Strong._
  override def freeSemiArrowProfunctorInstance[P[_, _]](implicit P: Strong[P]): Profunctor[FreeSemiArrow[P, ?, ?]] = new StrongClass.First[FreeSemiArrow[P, ?, ?]] {

    override def dimap[A, B, C, D](fab: FreeSemiArrow[P, A, B])(ca: C => A)(bd: B => D): FreeSemiArrow[P, C, D] =
      rmap(lmap(fab)(ca))(bd)

    override def lmap[A, B, C](fab: FreeSemiArrow[P, A, B])(ca: C => A): FreeSemiArrow[P, C, B] = 
      fab match {
        case ArrComposed(faz, fzb) => 
          val y = lmap(faz)(ca) 
          ArrComposed(y, fzb)
        case Lifted(pab) => Lifted(pab.lmap(ca))
      }
  
    override def rmap[A, B, D](fab: FreeSemiArrow[P, A, B])(bd: B => D): FreeSemiArrow[P, A, D] = 
      fab match {
        case Lifted(pab) => Lifted(pab.rmap(bd))
        case ArrComposed(faz, -\/(fzb)) => ArrComposed(faz, -\/[FreeSemiArrow[P, Any, D], Function1[Any, D]](rmap(fzb)(bd))) 
        case ArrComposed(faz, \/-(ffzb)) => 
          val x = \/-[FreeSemiArrow[P, Any, D], Function1[Any, D]](ffzb andThen bd)
          ArrComposed(faz, x) 
    }
  
    //override def trrmap[A, B, D](fab: FreeSemiArrow[P, A, B])(bd: B => D): FreeSemiArrow[P, A, D] = {
    //  def rec(fp: FreeSemiArrow[P, A, Z], fzb: FreeSemiArrow[P, Any, B]): FreeSemiArrow[P, A, D] = fzb match {
    //    case ArrComposed(faz, -\/(moar)) => rec(faz, moar) 
    //    case Arrcomposed(faz, -\/(Lifted(pzb)) => ArrComposed(faz, -\/(Lifted(pzb.rmap(bd)))) 
    //    case Lifted(pab) = Lifted(pab.rmap(bd))
    //    case ArrComposed(faz, \/-(ffzb)) => 
    //      val x = \/-[FreeSemiArrow[P, Any, D], Function1[Any, D]](ffzb andThen bd)
    //      ArrComposed(faz, x) 

    //  }
    //}

  }
}
