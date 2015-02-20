package scalaz.bench

import org.openjdk.jmh.annotations.{Benchmark, Scope, State}
import scalaz._
import scalaz.Free._

@State(Scope.Benchmark)
class FreeBench {

val chars: List[String] = ('a' to 'z').map(_.toString).toList

  val longSeq = (0 to 100000)

  def seqToFree(s: Seq[Int]) = s.map(ii => scalaz.State[Int,Int](i => (i,ii)).liftF ).foldLeft( scalaz.State[Int,Int](i => (i,0)).liftF )( (s,a) => s.flatMap(i => a.map(ii => i )))

  /** Benchmark fold of Foldable[List] */
  @Benchmark def freeFoldRun(): Int =
    seqToFree(longSeq).foldRun(0)((a,b) => b(a))._2
  
  /** Benchmark fold of Foldable[List] */
  @Benchmark def freeFoldRunOld(): Int =
    seqToFree(longSeq).foldRunOld(0)((a,b) => b(a))._2


}

