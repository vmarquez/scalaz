package scalaz

trait Cofoldable[F[_], A] {
  //we capture the type so we can also capture Type class constraints on the actual type  for things such as Set[A] needing an Ordering

  def build[B](b: B)(f: B => (A, Option[B])): F[A] //(F[A], Option[B]) mgiht allow partial forlds?

  //DERIVED 
 
  def fromIList(l: IList[A]): Option[F[A]] = 
    l match {
      case INil() => None
      case ICons(h, t) => 
        Some(build[IList[A]](t)(_ match {
        case ICons(hh, tt) => println("icons in fromIList blah")
          (hh, Some(tt))
        case INil() => (h, None) 
      }))
    }

  
  def filter(fa: F[A], f: A => Boolean)(implicit F: Foldable[F]): Option[F[A]] = 
    fromIList(F.toIList(fa).filter(f))

  def collect[B](fa: F[A], pf: PartialFunction[A, A])(implicit F: Foldable[F]): Option[F[A]] = 
    fromIList(F.toIList(fa).collect(pf))

   
  trait CofoldLaw {
    def toListWeakIdempotence[F[_], A](fa: F[A])(implicit F: Foldable[F], G: Cofoldable[F, A], E: Equal[IList[A]]): Boolean = {
      val tol: F[A] => IList[A] = (fa: F[A]) => G.fromIList(F.toIList(fa)).map(F.toIList(_)).getOrElse(IList[A]())
      E.equal(tol(fa), F.toIList(fa)) 
    }

    def fromListWeakIdempotence[F[_], A](fa: F[A])(implicit F: Foldable[F], G: Cofoldable[F, A], E: Equal[Option[F[A]]]): Boolean = {
      val fl: IList[A] => Option[F[A]] = (il: IList[A]) => G.fromIList(G.fromIList(il).map(F.toIList(_)).getOrElse(IList[A]()))
      E.equal(fl(F.toIList(fa)), G.fromIList(F.toIList(fa))) 
    }
  }
  
  val cofoldLaw = new CofoldLaw {}

}

object Cofoldable extends Instances {
  @inline def apply[F[_], A](implicit F: Cofoldable[F, A]): Cofoldable[F, A] = F

}

trait Instances {

  implicit def listCofoldable[A](implicit l: IList[A]): Cofoldable[IList, A] = new Cofoldable[IList, A] {
    def build[B](b: B)(f: B => (A, Option[B])): IList[A] = {
      def unfold(bb: B, il: IList[A]): IList[A] = f(bb) match {
        case (a, Some(b)) => unfold(b, a :: il) 
        case (a, None) => a :: il 
      }
      unfold(b, INil())
    }
  }

  implicit def dlistCofoldable[A](implicit dl: DList[A]) = new Cofoldable[DList, A] {
    def build[B](b: B)(f: B => (A, Option[B])): DList[A] = {
      def unfold(b: B, d: DList[A]): DList[A] = f(b) match {
        case (a, Some(b)) => unfold(b, a +: d) 
        case (a, None) => a +: d
      }
      unfold(b, DList())
    }
  }

  implicit def nelCofoldable[A](): Cofoldable[NonEmptyList, A] = new Cofoldable[NonEmptyList, A] {
    def build[B](b: B)(f: B => (A, Option[B])): NonEmptyList[A] = {
      def unfold(b: B, n: NonEmptyList[A]): NonEmptyList[A] = f(b) match {
        case (a, Some(bb)) => unfold(bb, a <:: n)
        case (a, None) => a <:: n
      }
      f(b) match {
        case (a, Some(b)) => unfold(b, NonEmptyList(a))
        case (a, None) => NonEmptyList(a)
      }
    }
  }
  
  def productCofoldable[F[_], G[_], A](implicit fa: F[A], ga: G[A], F: Cofoldable[F, A], G: Cofoldable[G, A]): Cofoldable[({ type l[a] = (F[A],G[A])})#l, A] = new Cofoldable[({ type l[a] = (F[A],G[A])})#l, A] {
    def build[B](b: B)(f: B => (A, Option[B])): (F[A], G[A]) = {
      (F.build(b)(f), G.build(b)(f)) 
    }
  }

  implicit def ISetCofoldable[A](implicit o: Order[A]): Cofoldable[ISet, A] = new Cofoldable[ISet, A] {
    def build[B](b: B)(f: B => (A, Option[B])): ISet[A] = {
      def unfold(b: B, is: ISet[A]): ISet[A] = f(b) match {
        case (a, Some(b)) => unfold(b, is.insert(a)) 
        case (a, None) => is.insert(a)
      }
      unfold(b, ISet.empty[A]) 
    }
  }
  /*
  def ICofreeCofoldable[F[_], A](implicit F: MonadPlus[F]): Cofoldable[({ type l[a] = Cofree[F, a]})#l] = new Cofoldable[({ type l[a] = Cofree[F, a]})#l] {
    def cofold[b](b: B)(f: B => Option[(A, B)]): Cofree[F, A] = {
      Cofree.unfold[F, A, B](b)(b => {
        f(b) match {
          case Some((a, b)) => 
          case None =>  
        }
      })
    }
  }*/
}

