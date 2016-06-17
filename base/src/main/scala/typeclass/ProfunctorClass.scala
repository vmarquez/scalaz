package scalaz
package typeclass

trait ProfunctorClass[P[_,_]] extends Profunctor[P] {
  final def profunctor: Profunctor[P] = this
}
