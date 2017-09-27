package scalaz
package data

//
//import scala.annotation.tailrec
//import scalaz.typeclass.Compose
//
//object FreeSemiArrowInstances {
//  import typeclass.{Profunctor, Strong, Category, StrongClass, CategoryClass}
//  import typeclass.Strong._
//  import typeclass.Profunctor._
//  import FreeSemiArrow._
////with CategoryClass[ProCompose[P, ?, ?]] { 
//
//  implicit def FreeSemiArrowStrong[P[_, _]: Strong]: StrongClass.First[FreeSemiArrow[P, ?, ?]] = new StrongClass.First[FreeSemiArrow[P, ?, ?]]  {
//    
//    override def dimap[A, B, C, D](fab: FreeSemiArrow[P, A, B])(fca: C => A)(fbd: B => D): FreeSemiArrow[P, C, D] = ???
//      //fab match {
//      //  case Chain(pab, more) => Chain(pab.lmap(fca), rmap(more)(fbd))
//      //  case Lift(f) => Lift(f.dimap(fca)(fbd))
//      //}   
//
//    override def rmap[A, B, D](fab: FreeSemiArrow[P, A, B])(fbd: B => D): FreeSemiArrow[P, A, D] =
//      fab match {
//        case Chain(pab, more) => Chain(pab, rmap(more)(fbd))
//        case Lift(f) => Lift(f.rmap(fbd))
//      }  
//
//    override def lmap[A, B, C](fab: FreeSemiArrow[P, A, B])(fca: C => A): FreeSemiArrow[P, C, B] =
//      fab match {
//        case Chain(pab, more) => Chain(pab.lmap(fca), more)
//        case Lift(f) => Lift(f.lmap(fca))
//      }   
//  
//    override def first[A, B, C](fab: FreeSemiArrow[P,A,B]): FreeSemiArrow[P,(A, C),(B, C)] = 
//      fab match {
//        case Chain(pab, more) => Chain(Strong[P].first(pab), first(more))
//        case Lift(f) => Lift(Strong[Function].first(f))
//      }   
//
//    override def second[A, B, C](fab: FreeSemiArrow[P,A,B]): FreeSemiArrow[P,(C, A),(C, B)] = 
//      fab match {
//        case Chain(pab, more) => Chain(Strong[P].second(pab), second(more))
//        case Lift(f) => Lift(Strong[Function].second(f))
//      }
//
//    //override def id[A]: FreeSemiArrow[P,A,A] = Pure((a: A) => a)
//    //
//    //override def compose[A, B, C](fbc: FreeSemiArrow[P, B, C], fab: FreeSemiArrow[P, A, B]): FreeSemiArrow[P, A, C] = fab match {
//    //    case Composed(pab, more) =>  
//    //      Composed(pab, compose(fbc, more)) 
//    //    case Pure(f) => lmap(fbc)(f) 
//    //  }
//  }
//
//}
//
//
///**
// * Non-empty type-aligned sequence represented as a (non-balanced) binary tree,
// * supporting O(1) addition to either end and O(1) concatenation.
// */
//sealed abstract class FreeSemiArrow[=>:[_, _], A, B] {
//  import FreeSemiArrow._
//
//  //should come from having a Category 
//  def compose[Z](that: FreeSemiArrow[=>:, Z, A]): FreeSemiArrow[=>:, Z, B] =
//    Chain(that, this)
//
//  //def <<<[Z](that: FreeSemiArrow[=>:, Z, A]): FreeSemiArrow[=>:, Z, B] =
//  //  this compose that
//
//  def >>>[C](that: FreeSemiArrow[=>:, B, C]): FreeSemiArrow[=>:, A, C] =
//    that compose this
//
//  def :+[C](f: B =>: C): FreeSemiArrow[=>:, A, C] =
//    Chain(this, Lift(f))
//
//  def +:[Z](f: Z =>: A): FreeSemiArrow[=>:, Z, B] =
//    Chain(Lift(f), this)
//
//  //final def foldLeft[F[_]](fa: F[A])(φ: RightAction[F, =>:]): F[B] = {
//  //  @inline def pair[X](fx: F[X], f: FreeSemiArrow[=>:, X, B]) = APair[F, FreeSemiArrow[=>:, ?, B], X](fx, f)
//  //  @tailrec def go(step: APair[F, FreeSemiArrow[=>:, ?, B]]): F[B] =
//  //    step._2 match {
//  //      case Chain(Chain(f, g), h) => go(pair(step._1, f >>> (g >>> h)))
//  //      case Chain(Lift(f), g) => go(pair(φ.apply(step._1, f), g))
//  //      case Lift(f) => φ.apply(step._1, f)
//  //    }
//  //  go(pair(fa, this))
//  //}
//
//  //def foldRight[F[_]](fb: F[B])(φ: LeftAction[F, =>:]): F[A] = {
//  //  @inline def pair[X](f: FreeSemiArrow[=>:, A, X], fx: F[X]) = APair[FreeSemiArrow[=>:, A, ?], F, X](f, fx)
//  //  @tailrec def go(step: APair[FreeSemiArrow[=>:, A, ?], F]): F[A] =
//  //    step._1 match {
//  //      case Chain(f, Chain(g, h)) => go(pair((f >>> g) >>> h, step._2))
//  //      case Chain(f, Lift(g)) => go(pair(f, φ.apply(g, step._2)))
//  //      case Lift(f) => φ.apply(f, step._2)
//  //    }
//  //  go(pair(this, fb))
//  //}
//
//  /**
//   * Compose the leafs in a balanced binary fashion.
//  @tailrec
//  final def fold(implicit ev: Compose[=>:]): A =>: B = this match {
//    case Chain(Chain(f, g), h) => (f >>> (g >>> h)).fold
//    case Chain(Lift(f), g) => g.foldLeft[PostComposeBalancer[=>:, A, ?]](PostComposeBalancer(f))(PostComposeBalancer.rightAction).result
//    case Lift(f) => f
//  }
//   */
//}
//
//object FreeSemiArrow {
//  //private[FreeSemiArrow]
//  final case class Lift[=>:[_, _], A, B](f: A => B) extends FreeSemiArrow[=>:, A, B]
//  //private[FreeSemiArrow] 
//  final case class Chain[=>:[_, _], A, B, C](f: FreeSemiArrow[=>:, A, B], g: FreeSemiArrow[=>:, B, C]) extends FreeSemiArrow[=>:, A, C]
//
//  def lift[F[_, _], A, B](f: A => B): FreeSemiArrow[F, A, B] =
//    Lift(f)
//}


abstract class FreeSemiArrow[P[_, _], A, B] {
  //def compose[C](pbc: P[B, C])
// def compose[Z](that: FreeSemiArrow[=>:, Z, A]): FreeSemiArrow[=>:, Z, B] =
  //    Chain(that, this)

  def compose[Z](fza: P[Z, A]): FreeSemiArrow[P, Z, A] =
    Composed(fza, this)

  def andThen[C](fbc: F[B, C]): FreeSemiArrow[P, A, C] =
    fbc compose this

  def fold(implicit ev: Compose[P]): P[A, B] = this match {
    case Composed(pab, more) => 
    case Lifted(fab) => 
    case LiftedFunction(f) =>  
  }

}


object FreeSemiArrow {
  //import typeclass.{Profunctor, Strong, Category, StrongClass, CategoryClass}
  import typeclass.Strong._
  import typeclass.Profunctor._

  
  case class LiftedFunction[F[_, _], A, B](f: A => B) extends FreeSemiArrow[F, A, B]

  case class Lifted[F[_, _], A, B](fab: F[A, B]) extends FreeSemiArrow[F, A, B]

  case class Composed[F[_, _], A, B, C](fab: FreeSemiArrow[F, A, B], fbc: FreeSemiArrow[F, B, C]) extends FreeSemiArrow[F, A, C]


  //def dimap[P[_, _], C, A, B, D](pab: SemiArrowOp[P, A, B])(ca: C => A)(bd: B => D): SemiArrowOp[P, C, D] = 
   // pab match {
    //  case Composed(
    //}

  
  //override def dimap[F[_, _], A, B, C, D](fab: SemiArrowOp[F, A, B])(fca: C => A)(fbd: B => D): SemiArrowOp[F, C, D] =
  //  fab match {
  //    case Composed(pab, more) =>
  //    case Lifted(_) => 
  //    case LiftedFunction(f) => 
  //  }
  
  def lmap[F[_, _], C, A, B](fab: SemiArrowOp[F, A, B])(ca: C => A): SemiArrowOp[F, C, B] = 
    fab match {
      case Composed(pab, more) => Composed(lmap(pab)(ca), more)
      case Lifted(_) => Composed(LiftedFunction(ca), fab)
      case LiftedFunction(f) => LiftedFunction(f.lmap(ca))
    }

  def rmap[F[_, _], A, B, D](fab: SemiArrowOp[F, A, B])(bd: B => D): SemiArrowOp[F, A, D] = 
    fab match {
      case Composed(pab, more) => Composed(pab, rmap(more)(bd))
      case Lifted(_) => Composed(fab, LiftedFunction(bd))
      case LiftedFunction(f) => LiftedFunction(bd compose f)
      
    }
}

//abstract class FreeSemiArrow[F[_, _], A, B] {
//}
