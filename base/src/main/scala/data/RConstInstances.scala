package scalaz
package data

import scalaz.typeclass._
import data.Disjunction.\/

trait RConstInstances {
  implicit def rconstProfunctor = new Profunctor[RConst] {
    override def lmap[A, B, C](fab: RConst[A, B])(ca: C => A): RConst[C, B] = fab.retag[C]
    override def rmap[A, B, C](fab: RConst[A, B])(bc: B => C): RConst[A, C] = RConst[A, C](bc(fab.rconst))
  }

  implicit def rconstChoice = new Choice[RConst] {
    override def profunctor = Profunctor[RConst]
    override def left[A, B, C](fab: RConst[A, B]): RConst[A \/ C, B \/ C] = ???

    override def right[A, B, C](fab: RConst[A, B]): RConst[C \/ A, C \/ B] = RConst[C \/ A, C \/ B](Disjunction.right(fab.rconst))

  }
}
