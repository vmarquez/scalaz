package scalaz
package data

final case class RConst[A, B](rconst: B) {
  def retag[C]: RConst[C, B] = this.asInstanceOf[RConst[C, B]]
}

object RConst extends RConstInstances
