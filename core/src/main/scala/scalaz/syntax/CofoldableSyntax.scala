package scalaz
package syntax

final class CofoldableOps[F[_], A](val self: F[A])(implicit val F: Cofoldable[F[A], A]) extends Ops[F[A]] {
  ////

  final def filterAll(f: A => Boolean)(implicit G: Foldable[F]): F[A] =
    Cofoldable.filterAll(self, f)

  final def collectall[B](pf: PartialFunction[A, A])(implicit G: Foldable[F]): F[A] =
    Cofoldable.collectall(self, pf)

  final def splitup(f: A => Boolean)(implicit G: Foldable[F]): (F[A], F[A]) = Cofoldable.splitup(self, f)

  ////
}

trait ToCofoldableOps[TC[F, A] <: Cofoldable[F, A]] {
  implicit def ToCofoldableOps[F[A], A](v: F[A])(implicit F0: TC[F[A], A]) =
    new CofoldableOps[F, A](v)
}

trait CofoldableSyntax[F[_], A] {
  implicit def ToCofoldableOps(fa: F[A]): CofoldableOps[F, A] = new CofoldableOps[F, A](fa)(CofoldableSyntax.this.F)

  def F: Cofoldable[F[A], A]
}
