package scalaz
package optics

import scalaz.typeclass._
import scalaz.data._
import Forget._


trait Lens[S, T, A, B] { self =>
  def stab[P[_, _]: Strong]: P[A, B] => P[S, T]

  def get(s: S): A = {
    val p = Strong[({ type l[a, b] = Forget[A, a, b]})#l]
    (stab[({ type l[a, b] = Forget[A, a, b]})#l])(p)(Forget[A, A, B](a => a)).forget(s)
  }
  
  def set(b: B, s: S): T = {
    val p = Strong[Function1]
    stab[Function1](p)((a: A) => b)(s)
  }

  def compose[C, D](lens: Lens[A, B, C, D]): Lens[S, T, C, D] = new Lens[S, T, C, D] {
    override def stab[P[_,_]: Strong]: P[C, D] => P[S, T] = (pcd: P[C, D]) => {
      val pab = lens.stab(Strong[P])(pcd)
      self.stab(Strong[P])(pab)
    }
  }
}

object Lens {
  
  def apply[S, T, A, B](get: S => A, set: (B, S) => T): Lens[S, T, A, B] = new Lens[S, T, A, B] {
    override def stab[P[_, _]: Strong]: P[A, B] => P[S, T] = (pab: P[A, B]) => { 
      val t = Strong[P].first[A, B, S](pab)
      Profunctor[P].dimap(t)((s: S) => (get(s),s ))(t => set(t._1, t._2))
    }
  }
  
  case class User(name: String, age: Int)
  case class Account(id: Int, user: User)
  
  val usernameLens = Lens((u: User) => u.name, (n: String, u: User) => u.copy(name = n))

  val accountuserLens = Lens((a: Account) => a.user, (u: User, a: Account) => a.copy(user = u))
 
  val accountusernameLens = accountuserLens compose usernameLens
  
  
  val  unwoundededCompose: Function1[String, String] => Function1[Account, Account] = (pab: Function1[String, String]) => {
    val t1 = Strong[Function1].first[String, String, User](pab)
    val utou = Profunctor[Function1].dimap(t1)((u: User) => (u.name, u))((t: (String, User)) => t._2.copy(name = t._1))
     
    val t2 = Strong[Function1].first[User, User, Account](utou)
    Profunctor[Function1].dimap(t2)((a: Account) => (a.user, a))((t: (User, Account)) => t._2.copy(user = t._1))
  }

  val accountusernameLensUnwound = (pab: Function1[User, User]) => {
      val t = Strong[Function1].first[User, User, Account](pab)
      Profunctor[Function1].dimap(t)((a: Account) => (a.user, a))((t:(User, Account)) => t._2.copy(user = t._1))
  }

  val usernameUnwound = (pab: Function1[String, String]) => {
    val t = Strong[Function1].first[String, String, User](pab)
    Profunctor[Function1].dimap(t)((u: User) => (u.name, u))((t:(String, User)) => t._2.copy(name = t._1))
  }

  
  //val accountUserNameLens = accountUserLens compose userNameLens //usserNameLens compose accountUserLens
  //val lens = 
}

