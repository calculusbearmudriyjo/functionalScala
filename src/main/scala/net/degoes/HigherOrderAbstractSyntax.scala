object HigherOrderAbstractSyntax {
  /**
    * Let's write a language which supports:
    *
    * let i = 0
    * in while (i < 10) { i = i + 1 }
    */
  trait Expr[F[_]] {
    def intLit(value: Int): F[Int]
    def add(l: F[Int], r: F[Int]): F[Int]
    // Uses scala lambdas in input type
    def let[A, B](name: Symbol, value: F[A], body: F[A] => F[B]): F[B]
    def updateVar[A](name: Symbol, value: F[A]): F[A]
    def lessThan(l: F[Int], Right: F[Int]): F[Boolean]
    def while0[A](condition: F[Boolean], body: F[A]): F[Unit]
  }
  object Expr {
    def apply[F[_]](implicit F: Expr[F]): Expr[F] = F
  }

  implicit class IntExprSyntax[F[_]](left: F[Int]) {
    def + (right: F[Int])(implicit F: Expr[F]): F[Int] = F.add(left, right)
    def < (right: F[Int])(implicit F: Expr[F]): F[Boolean] = F.lessThan(left, right)
  }
  def int[F[_]: Expr](i: Int): F[Int] = Expr[F].intLit(i)
  def let[F[_]: Expr, A, B](name: Symbol, value: F[A])(body: F[A] => F[B]): F[B] =
    Expr[F].let(name, value, body)
  def while0[F[_]: Expr, A](condition: F[Boolean])(body: F[A]): F[Unit] =
    Expr[F].while0(condition, body)

  case class IState(value: Map[Symbol, Any]) {
    def addVariable(name: Symbol, v: Any): IState =
      copy(value = value + (name -> v))

    def removeVariable(name: Symbol): IState =
      copy(value = value - name)
  }


  // Look how in this program, 'i is defined in let, but than becomes a real
  // scala variable in the places where you want to use it.
  //
  // Variables in the DSL, have now become variables in scala as well. This
  // doesn't hold for the updateVar yet. This can be worked around, but if
  def program[F[_]: Expr]: F[Unit] =
    let('i, int(0))(i =>
      while0(i < int(10))(
        Expr[F].updateVar('i, i + int(1))
      )
    )
}