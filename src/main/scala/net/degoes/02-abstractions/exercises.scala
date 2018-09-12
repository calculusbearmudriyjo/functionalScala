// Copyright(C) 2018 - John A. De Goes. All rights reserved.

package net.degoes.abstractions

import scalaz._
import Scalaz._
//import net.degoes.essentials.types.Employees

object algebra {

  /*
   * Associativity:
   * x |+| (y |+| z) === (x |+| y) |+| z
   */
//  trait Semigroup[A] {
//    def append(l: => A, r: => A): A
//  }
//  implicit class SymegroupSyntax[A](a: A) {
//    def |+| (r: A)(implicit s: Semigroup[A]): A = s.append(a, r)
//  }
//
//  case class Sum(value: Int)
//
//  implicit val SumSemigroup: Semigroup[Sum] = new Semigroup[Sum] {
//    def append(l: => Sum, r: => Sum): Sum = Sum(l.value + r.value)
//  }
//
//  Sum(1) |+| Sum(2)

  //
  // EXERCISE 1
  //
  // Define a semigroup for `NotEmpty` below.
  //
  case class NotEmpty[+A](head: A, tail: Option[NotEmpty[A]])
//  implicit def NotEmptySemigroup[A]: Semigroup[NotEmpty[A]] = new Semigroup[NotEmpty[A]] {
//    def append(l: NotEmpty[A], r: NotEmpty[A]): NotEmpty[A] = NotEmpty[A](l.head, l.tail match {
//      case None => Some(r)
//      case Some(l) => Some(append(l, r))
//    })
//  }

  val example1 = NotEmpty(1, None) |+| NotEmpty(2, None)

  //
  // EXERCISE 2
  //
  // Design a permission system for securing some resource, together with a
  // monoid for the permission data structure.
  //
  type Resource = String
  type Email = String

  sealed trait Capability
  case object Read extends Capability
  case object Write extends Capability
  case class Permission(value: Map[Email, Map[Resource, Set[Capability]]])
  // TODO HOME

  /*
    type Email = String
    type Resource = String

    sealed trait Capability
    case object Read extends Capability
    case object Write extends Capability

    case class Permission(value: Map[Email, Map[Resource, Set[Capability]]])
    object Permission {
    def apply() = {
    new Permission(Map.empty)
    }
    }
    implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission] {
    override def append(f1: Permission, f2: => Permission): Permission = {
    Permission(f1.value.mappend(f2.value))
    }
    override def zero: Permission = Permission(Map.empty)
    }


    implicit val MonoidPermission: Monoid[Permission] = new Monoid[Permission]{
    def zero: Permission = Permission(Map())
    def append(l: Permission, r: ⇒ Permission): Permission =
      Permission(l.values |+| r.values)
    }
   */
  implicit val MonoidPermission: Monoid[Permission] = ???
//  val example2 = mzero[Permission] |+| Permission()

  //
  // EXERCISE 3
  //
  // Define an instance of `Semigroup` for `(A, B)` when both `A` and
  // `B` form semigroups.
  //
  implicit def SemigroupTuple2[A: Semigroup, B: Semigroup]:
    Semigroup[(A, B)] = new Semigroup[(A, B)] {
    def append(f1: (A, B), f2: => (A, B)): (A, B) = (f1._1 |+| f2._1, f1._2 |+| f2._2)
  }

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Monoid` for `NotEmpty` for any type `A`.
  // CAN NOT BECAUSE NOTEMPTY LIST CAN'T BE EMPTY -> MONOID LAW
  implicit def MonoidNotEmpty[A]: Monoid[NotEmpty[A]] = ???
}

object functor {
  //
  // EXERCISE 1
  //
  // Define an instance of `Functor` for `BTree`.
  //

  implicit def mapFunctor[K]: Functor[Map[K, ?]] =
    new Functor[Map[K, ?]] {
      override def map[A, B](fa: Map[K, A])(f: A => B): Map[K, B] = fa.mapValues(f)
    }

  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]
  implicit val BTreeFunctor: Functor[BTree] =
    new Functor[BTree] {
      def map[A, B](fa: BTree[A])(f: A => B): BTree[B] =
        fa match {
          case Leaf(a) => Leaf(f(a))
          case Fork(l, r) => (l ,r) match {
            case (l: Leaf[A], r:Leaf[A]) => Fork(Leaf(f(l.a)), Leaf(f(r.a)))
            case (l: Leaf[A], r:Fork[A]) => Fork(Leaf(f(l.a)), map(r)(f))
            case (l: Fork[A], r:Fork[A]) => Fork(map(l)(f), map(r)(f))
            case (l: Fork[A], r:Leaf[A]) => Fork(map(l)(f), Leaf(f(r.a)))
          }
        }
    }

  //
  // EXERCISE 2
  //
  // Define an instance of `Functor` for `Nothing`.
  //
  implicit val NothingFunctor: Functor[Nothing] = ???

  //
  // EXERCISE 3
  //
  // Define an instance of `Functor` for Parser[E, ?].
  //
  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def ParserFunctor[E]: Functor[Parser[E, ?]] = ???

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Functor` for the following data type.
  //
  case class DataType[A](f: A => A)
  implicit val DataTypeFunctor: Functor[DataType] = ???

  //
  // EXERCISE 5
  //
  // Define an instance of `Functor` for `FunctorProduct`.
  //
  case class FunctorProduct[F[_], G[_], A](l: F[A], r: F[A])
  implicit def FunctorProductFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorProduct[F, G, ?]] = ???



/*  case class USD(amount: BigDecimal)

  case class Amount[A](value: A)
//  Amount[USD](12.22).map()
  case class Account[Id](id: Id)
  case class Client[F[_], A](accounts: F[Account[A]])
  sealed trait Operation[A] {
    def map[B](f: A => B): Operation[B] = map(this, f)
    def zip[B](that: Operation[B]): Operation[(A, B)] = Both(this, that)
  }
  case class Deposit[Id, A](to: Account[Id], amount: Amount[A]) extends Operation[Amount[A]]
  case class Withdraw[Id, A](from: Account[Id], amount: Amount[A]) extends Operation[Amount[A]]
  case class Both[A,B](l: Operation[A], r: Operation[B]) extends Operation[(A,B)]
  case class Map[A,B](op:Operation[A], f: A => B) extends Operation[B]

  val acc = Account(1)
  (Deposit(acc, Amount(12.22)) zip (Withdraw(acc, Amount(12.22)))).map {
    case (deposit, withdraw) => deposit.value - withdraw.value
  }
  def commit[A](op: Operation[A]):A = ???*/


  def zipOption[A,B](l: Option[A], r: Option[B]): Option[(A,B)] = (l, r) match {
    case (Some(a), Some(b)) => Some((a,b))
    case _ => None
  }

  def zipWith[A,B,C](l: Option[A], r: Option[B])(f: ((A, B)) => C): Option[(C)] =
    zipOption(l,r).map(f)

  def zipList1[A,B](l: List[A], r: List[B]): List[(A,B)] =
    (l, r) match {
      case (a :: as, bs) => zipList1(as, bs) ++ bs.map(b => (a, b))
      case (Nil, bs) => Nil
    }

  def zipList2[A,B](l: List[A], r: List[B]): List[(A,B)] =
    (l, r) match {
      case (a :: as, b :: bs) => (a, b) :: zipList2(as, bs)
      case _ => Nil
    }

  trait Apply[F[_]] extends Functor[F] {
    def zip[A,B](l: F[A], r:F[B]): F[(A,B)]
  }

  implicit class ApplySyntax[F[_],A](l: F[A]) {
    def *> [B](r: F[B])(implicit F: Apply[F]): F[B] = F.zip(l, r).map(_._2) // NOT EQ R

    def <* [B](r: F[B])(implicit F: Apply[F]): F[A] = F.zip(l, r).map(_._1) // NOT EQ L
  }

//  trait Applicative[F[_]] extends Apply[F] {
//    // fa <* fb !== fa
//    // fa *> fb !== fb
//    // fa <* point(b) === fa
//    // point(b) *> fa === fa
//    def point[A](a: => A): F[A]
//  }
//
//  val l = List(1,2,3)
//  val r = List(9,2)
//  val lr1 = List((1,9), (1,2), (2,9), (2,2), (3,9), (3,2))
//  val lr2 = List((1,9), (2,2))
//
//  val lr1_mapped = lr1.map(_._1)
//  // l <* r
//  //List(1, 1, 2, 2, 3, 3)
//  val lr1_mapped2 = lr1.map(_._2)
  // l *> r
  //List(9, 2, 9, 2, 9, 2)
  // ETC


  //
  // EXERCISE 6
  //
  // Define an instance of `Functor` for `FunctorSum`.
  //
  case class FunctorSum[F[_], G[_], A](run: Either[F[A], G[A]])
  implicit def FunctorSumFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorSum[F, G, ?]] = ???

  //
  // EXERCISE 7
  //
  // Define an instance of `Functor` for `FunctorNest`.
  //
  case class FunctorNest[F[_], G[_], A](run: F[G[A]])
  implicit def FunctorNestFunctor[F[_]: Functor, G[_]: Functor]:
    Functor[FunctorNest[F, G, ?]] = ???

  //
  // EXERCISE 8
  //
  // Define `Applicative` for `Option`.
  //
//  implicit val OptionApplicative: Applicative[Option] =
//    new Applicative[Option] {
//      def point[A](a: => A): Option[A] = Some(a)
//      def ap[A, B](fa: => Option[A])(f: => Option[A => B]): Option[B] = fa match {
//        case None => None
//        case Some(e) => f(e)
//      }
//    }

  //
  // EXERCISE 9
  //
  // Implement `zip` in terms of the applicative composition using `|@|`.
  //
  val example1 = (Option(3) |@| Option(5))((_, _))
  val example2 = zip(Option(3), Option("foo")) : Option[(Int, String)]
  def zip[F[_]: Applicative, A, B](l: F[A], r: F[B]): F[(A, B)] =
    ???
  def ap2[F[_]: Applicative, A, B](fa: F[A], fab: F[A => B]): F[B] =
    ???

  //
  // EXERCISE 10
  //
  // Define an instance of `Applicative` for `Parser[E, ?]`.
  //
  implicit def ApplicativeParser[E]: Applicative[Parser[E, ?]] =
    new Applicative[Parser[E, ?]] {
      def point[A](a: => A): Parser[E,A] = ???

      def ap[A, B](fa: => Parser[E,A])(
        f: => Parser[E, A => B]): Parser[E,B] = ???
    }

  trait Monad[F[_]] extends Applicative[F] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Future[+A] { self =>
    def flatMap[B](f: A => Future[B]): Future[B] = ???
  }
  //
  // EXERCISE 11
  //
  // Define an instance of `Monad` for `BTree`.
  //
  implicit val MonadBTree: Monad[BTree] = new Monad[BTree] {
    override def bind[A, B](fa: BTree[A])(f: A => BTree[B]): BTree[B] = fa match {
      case Leaf(a) => f(a)
      case Fork(l ,r) => Fork(bind(l)(f), bind(r)(f))
    }

    override def point[A](a: => A): BTree[A] = Leaf(a)
    override def ap[A, B](fa: => BTree[A])(f: => BTree[A => B]): BTree[B] = ???
  }

  //
  // EXERCISE 12
  //
  // Define an instance of `Monad` for `Parser[E, ?]`.
  //
  implicit def MonadParser[E]: Monad[Parser[E, ?]] = new Monad[Parser[E, ?]] {
    override def bind[A, B](fa: Parser[E, A])(f: A => Parser[E, B]): Parser[E, B] =
      Parser {
        fa.run(_) match {
          case Left(value) ⇒ Left(value)
          case Right((str, a)) ⇒ f(a).run(str)
        }
      }

    override def point[A](a: => A): Parser[E, A] = ???
    override def ap[A, B](fa: => Parser[E, A])(f: => Parser[E, A => B]): Parser[E, B] = ???
  }
  /*
   * Functor - Gives us the power to map values produced by programs without changing their structure
   * Apply - Adds the power to combine two programs into one by combining their values
   * Applicative - Adds the power to produce a 'pure' program that produces the given result
   * Monad - Adds the power to feed the result of one program into a function, which can look at the runtime value and return a new program, which is used to produce the result of the bind
   */
}

object foldable {
  //
  // EXERCISE 1
  //
  // Define an instance of `Foldable` for `BTree`.
  //
  sealed trait BTree[+A]
  case class Leaf[A](a: A) extends BTree[A]
  case class Fork[A](left: BTree[A], right: BTree[A]) extends BTree[A]

  /*
      def foldMap[A,B](fa: BTree[A])(f: A => B)(implicit  F: scalaz.Monoid[B]): B =
      fa match {
        case Leaf(a) => f(a)
        case Fork(l, r) => foldMap(l)(f) |+| foldMap(r)(f)
      }

    def foldRight[A, B](fa: BTree[A], z: => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(a) => f(a, z)
        case Fork(l, r) => foldRight(l,foldRight(r, z)(f))(f)
      }
   */
  //
  // EXERCISE 2
  //
  // Try to define an instance of `Foldable` for `A => ?`.
  //
//  implicit def FunctionFoldable[A]: Foldable[A => ?] = new Foldable[A => ?] {
//    def foldMap[A, B](fa: A => A)(f: A => B)(implicit F: Monoid[B]): B =
//      mzero[B]
//    def foldRight[A, B](fa: A => A, z: => B)(f: (A, B) => B): B =
//      z
//  }

  //
  // EXERCISE 3
  //
  // Define an instance of `Traverse` for `BTree`.
  //
  implicit val TraverseBTree: Traverse[BTree] = new Traverse[BTree] {
    def traverseImpl[G[_], A, B](fa: BTree[A])(f: A => G[B])(implicit F: Applicative[G]): G[BTree[B]] =
      fa match {
        case Leaf(a) => f(a).map[BTree[B]](Leaf(_))
        case Fork(l, r) =>
          val lg: G[BTree[B]] = traverseImpl(l)(f)
          val rg: G[BTree[B]] = traverseImpl(r)(f)
          (lg |@| rg)((l: BTree[B], r: BTree[B]) => Fork(l, r))
      }
  }


  type Future[A] = List[A]
  type Response = String
  def collectAllAPIs(results: BTree[Future[Response]]): Future[BTree[Response]] = results.sequence

  //
  // EXERCISE 4
  //
  // Try to define an instance of `Traverse` for `Parser[E, ?]`.
  //

  case class Parser[+E, +A](run: String => Either[E, (String, A)])
  implicit def TraverseParser[E]: Traverse[Parser[E, ?]] = ???
}

object optics {
  sealed trait Country
  case object Usa extends Country
  case object UK extends Country

  case class Org(name: String, address: Address, site: Site)
  case class Address(
                    number: String,
                    street: String,
                    postalCode: String,
                    country: Country)

  case class Site(manager: Employee, address: Address, employees: Set[Employee])
  case class Employee(name: String, dob: java.time.Instant, salary: BigDecimal, address: Address)


  object Org {
    val site: Lens[Org, Site] = Lens[Org, Site](_.site, l => _.copy(site = l))
  }

  object Site {
    val manager: Lens[Site, Employee] = Lens[Site, Employee](_.manager, m => _.copy(manager = m))
  }

  object Employee {
    val salary: Lens[Employee, BigDecimal] = Lens[Employee, BigDecimal](_.salary, s => _.copy(salary = s))
  }

  lazy val org: Org = ???
  lazy val org2: Org =
    org.copy(site =
      org.site.copy(manager = org.site.manager.copy(
        salary = org.site.manager.salary * 0.95
    )))

  type Optic[S, A]

  // S - Superstructure
  // A - Sub structure
  // An Optic S A allows you to focus in on a sub structure
  // A inside a super structure S, for purposes of accessing
  // or modifying the substructure.

  // EXERCISE 1
  case class Lens[S, A](get: S => A, set: A => (S => S)) { self =>
    def >>> [B](that: Lens[A, B]): Lens[S,B] = Lens[S, B](
      get = (s: S) => that.get(self.get(s)),
      set = (b: B) => (s: S) => self.set(that.set(b)(self.get(s)))(s)
    )

    final def update(f: A => A): S => S = (s: S) => self.set(f(self.get(s)))(s)
  }

  // EXERCISE 2
  //
  //

  val org2_lens =
    (Org.site >>> Site.manager >>> Employee.salary)
      .update(_ * 0.95)(org)


  // EXERCISE 3
  //
  //
  final case class Prism[S, A](
                              get: S => Option[A],
                              set: A => S) { self =>
    def >>> [B](that: Prism[A, B]): Prism[S, B] =
      Prism[S,B](
        get = (s: S) => self.get(s).flatMap(that.get),
        set = self.set.compose(that.set)
      )
  }

  // EXERCISE 4
  def _Left[A, B]: Prism[Either[A, B], A] =
    Prism[Either[A,B], A]({
      case Left(a) => Some(a)
      case _ => None
    }, Left(_))

  def _Right[A, B]: Prism[Either[A, B], B] =
    Prism[Either[A,B], B]({
      case Right(b) => Some(b)
      case _ => None
    }, Right(_))

    /*
    Polymorphic state

    case class Component1[S](lens: Lens[S, Component1.State]) {
    def run[S](state: S): (S, Boolean) = ???
  }
  object Component1 {
    case class Config(server: String, port: Int)
    case class State(config: Config)
  }
  case class Component2[S](lens: Lens[S, Component2.State]) {
    def run[S](state: S): (S, Int) = ???
  }
  object Component2 {
    case class Config(loggingDirectory: String)
    case class State(config: Config)
  }
  case class MyAppState(
    c1: Component1.State,
    c2: Component2.State
  )
  object MyAppState {
    val c1: Lens[MyAppState, Component1.State] = ???
    val c2: Lens[MyAppState, Component2.State] = ???
  }
  val c1 : Component1[MyAppState] = Component1(MyAppState.c1)
  val c2 : Component2[MyAppState] = Component2(MyAppState.c2)
     */
}
