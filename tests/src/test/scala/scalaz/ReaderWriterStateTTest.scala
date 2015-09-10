package scalaz

import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop.forAll

object ReaderWriterStateTTest extends SpecLite {
  type RWSOptInt[A] = RWST[Option, Int, Int, Int, A]
  implicit val RWSOptIntArb = Arbitrary(Gen.oneOf[RWSOptInt[Int]](
    Gen.const(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => None)),
    Gen.const(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((0, 0, 0)))),
    Gen.const(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((r, r, r)))),
    Gen.const(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((s, s, s))))
  ))
  implicit val RWSOptIntIntArb = Arbitrary(Gen.oneOf[RWSOptInt[Int => Int]](
    Gen.const(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => None)),
    Gen.const(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((0, x => 0, 0)))),
    Gen.const(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((r, x => r, r)))),
    Gen.const(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((s, x => s, s)))),
    Gen.const(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((s, x => x, s))))
  ))

  implicit val RWSOptIntEqual = new Equal[RWSOptInt[Int]] {
    def equal(a1: RWSOptInt[Int], a2: RWSOptInt[Int]) = a1.run(0, 0) == a2.run(0, 0)
  }

  checkAll(monad.laws[RWSOptInt])

  object instances {
    def functor[F[_]: Functor, R, W, S] = Functor[RWST[F, R, W, S, ?]]
    def monad[F[_]: Monad, R, W: Monoid, S] = Monad[RWST[F, R, W, S, ?]]
    def bind[F[_]: Bind, R, W: Semigroup, S] = Bind[RWST[F, R, W, S, ?]]
    def monadReader[F[_]: Monad, R, W: Monoid, S] = MonadReader[RWST[F, ?, W, S, ?], R]
    def monadState[F[_]: Monad, R, W: Monoid, S] = MonadState[RWST[F, R, W, ?, ?], S]
    def monadTrans[R, W: Monoid, S] = MonadTrans[λ[(f[_], α) => RWST[f, R, W, S, α]]]
    // checking absence of ambiguity
    def functor[F[_]: Monad, R, W: Monoid, S] = Functor[RWST[F, R, W, S, ?]]
    def functor[F[_]: Bind, R, W: Semigroup, S] = Functor[RWST[F, R, W, S, ?]]
  }
}
