package scalaz

trait Cofoldable[F[_], A]  {
  
  //we capture the type so we can also capture Type class constraints on the actual type  for things such as Set[A] needing an Ordering
  
  protected val foldable: Foldable[F] //not used from here, just to ensure we can't make a Cofoldable without a Foldable

  def build[B](b: B)(f: B => Option[(A, B)]): F[A]

  //DERIVED 
 
  def fromIList(l: IList[A]): F[A] = 
    build[IList[A]](l)(_ match {
      case ICons(hh, tt) => Some((hh, tt))
      case INil() => None
    })
  
  def filterAll(fa: F[A], f: A => Boolean)(implicit F: Foldable[F]): F[A] = 
    fromIList(F.toIList(fa).filter(f))

  def collectall[B](fa: F[A], pf: PartialFunction[A, A])(implicit F: Foldable[F]): F[A] = 
    fromIList(F.toIList(fa).collect(pf))

  def splitup(fa: F[A], f: A => Boolean)(implicit F: Foldable[F]): (F[A], F[A]) = {
    val t = F.toIList(fa).partition(f)
    (fromIList(t._1), fromIList(t._2))
  }

   
  trait CofoldLaw {
    
    //lwas will prevent instances from creating instances that non parametrically generate As...

    def toListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F, A], E: Equal[IList[A]]): Boolean = {
      val tol: F[A] => IList[A] = (fa: F[A]) => G.foldable.toIList(G.fromIList(G.foldable.toIList(fa).reverse))
      E.equal(tol(fa), G.foldable.toIList(fa)) 
    }

    def fromListWeakIdempotence[F[_], A](fa: F[A])(implicit G: Cofoldable[F, A], E: Equal[F[A]]): Boolean = {
      val fl: IList[A] => F[A] = (il: IList[A]) => G.fromIList(G.foldable.toIList(G.fromIList(il)))
      E.equal(fl(G.foldable.toIList(fa).reverse), G.fromIList(G.foldable.toIList(fa))) 
    }
  }
  
  val cofoldLaw = new CofoldLaw {}
}

object Cofoldable {
  @inline def apply[F[_], A](implicit F: Cofoldable[F, A]): Cofoldable[F, A] = F
}
