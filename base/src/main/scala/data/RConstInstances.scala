package scalaz
package data

import scalaz.typeclass._

trait RConstInstances {
  implicit def rconstProfunctor = new Profunctor[RConst] {
    override def lmap[A, B, C](fab: RConst[A, B])(ca: C => A): RConst[C, B] = fab.retag[C]
    override def rmap[A, B, C](fab: RConst[A, B])(bc: B => C): RConst[A, C] = RConst[A, C](bc(fab.rconst))
    override def dimap[A, B, C, D](fab: RConst[A, B])(ca: C => A)(bd: B => D): RConst[C, D] = RConst[C, D](bd(fab.rconst))
  }
}
