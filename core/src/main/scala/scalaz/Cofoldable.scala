package scalaz
////
/**
* Cofoldable type class implying the ability to ingest zero or more
* values to produce a type.
*/
////
trait Cofoldable[F, A] extends Cofoldable1[F, A] {
  ////   
  
  def unfoldr[B](b: B)(f: B => Option[(B, A)]): F
  
  def unfoldr1[B](b: B)(f: B => (A, Option[B])): F =
    unfoldr(b)(b => f(b) match {
      case (a, Some(b)) => Some((b, a))
      case (a, None) => None
    })
  
  /** DERIVED */ 
  def fromIList(l: IList[A]): F =
    unfoldr[IList[A]](l)(_ match {
      case ICons(h, t) => Some((t, h))
      case INil() => None
    })
   
  trait CofoldLaw {
    
    //lwas will prevent instances from creating instances that non parametrically generate As...

    def toListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F[A], A], E: Equal[IList[A]], F: Foldable[F]): Boolean = {
      val tol: F[A] => IList[A] = (fa: F[A]) => F.toIList(G.fromIList(F.toIList(fa)))
      E.equal(tol(fa), F.toIList(fa)) || E.equal(tol(fa).reverse, F.toIList(fa)) //we shoulnd't care what direction we add to the list 
    }

    def fromListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F[A], A], E: Equal[F[A]], F: Foldable[F]): Boolean = {
      val fl: IList[A] => F[A] = (il: IList[A]) => G.fromIList(F.toIList(G.fromIList(il)))
      E.equal(fl(F.toIList(fa).reverse), G.fromIList(F.toIList(fa))) 
    }
  }
  val cofoldLaw = new CofoldLaw {}

  ////
  //val cofoldSyntax = new scalaz.syntax.CofoldableSyntax[F, A] { def F = Cofoldable.this }
}

object Cofoldable {
  @inline def apply[F, A](implicit F: Cofoldable[F, A]): Cofoldable[F, A] = F
  //@inline def apply[F[_], A](implicit F: Cofoldable[F[A], A]): Cofoldable[F[A], A] = F

  def filterAll[F[_], A](fa: F[A], f: A => Boolean)(implicit CF: Cofoldable[F[A], A], F: Foldable[F]): F[A] =
    CF.fromIList(F.toIList(fa).filter(f))

  def collectall[F[_], A, B](fa: F[A], pf: PartialFunction[A, A])(implicit CF: Cofoldable[F[A], A], F: Foldable[F]): F[A] =
    CF.fromIList(F.toIList(fa).collect(pf))

  def splitup[F[_], A](fa: F[A], f: A => Boolean)(implicit CF: Cofoldable[F[A], A], F: Foldable[F]): (F[A], F[A]) = {
    val t = F.toIList(fa).partition(f)
    (CF.fromIList(t._1), CF.fromIList(t._2))
  }

}


