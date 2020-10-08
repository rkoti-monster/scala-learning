package io.example.one

object Driver3 extends App {

  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

    def map[B](f: A => B): IO[B] = flatMap {
      a => Return(f(a))
    }
  }

  object IO {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)

    def forever[A, B](a: IO[A]): IO[B] = {
      lazy val t: IO[B] = forever(a)
      a flatMap (_ => t)
    }
  }

  case class Return[A](a: A) extends IO[A]

  case class Suspend[A](resume: () => IO[A]) extends IO[A]

  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

  def printLine(s: String): IO[Unit] = Suspend(() => Return(println(s)))

  val p = IO.forever(printLine("Still going..."))

  //run(p)

  val f: Int => IO[Int] = (x: Int) => Return(x + 2)

  val g = List.fill(100000)(f).foldLeft(f) {
    (a, b) => x => Suspend(() => a(x).flatMap(b))
  }

  println(run(g(42)))

  def fac(n: Int): IO[Int] = {
    if (n == 0) Return(1)
    else FlatMap[Int, Int](Suspend(() => fac(n - 1)), x => Return(n * x))
  }

  run(fac(10).map(_.toString).flatMap(printLine))

  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => run(r())
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(r() flatMap f)
      case FlatMap(y, g) => run(y flatMap (g(_) flatMap f))
    }
  }

  // run(p)

  def unsafeEven(n: Int): Boolean = {
    if (n == 0) true
    else unsafeOdd(n - 1)
  }

  def unsafeOdd(n: Int): Boolean = {
    if (n == 0) false
    else unsafeEven(n - 1)
  }

  def safeEven(n: Int): IO[Boolean] = {
    if (n == 0) Return(true)
    else Suspend(() => safeOdd(n - 1))
  }

  def safeOdd(n: Int): IO[Boolean] = {
    if (n == 0) Return(false)
    else Suspend(() => safeEven(n - 1))
  }

  run(safeEven(1000000).map(_.toString).flatMap(printLine))

  def unsafeFib(n: Int): Int = {
    if (n <= 1) n
    else unsafeFib(n - 2) + unsafeFib(n - 1)
  }

  println(unsafeFib(25))

  def fib(n: Int): IO[Int] = {
    if (n <= 1) Return(n)
    else Suspend(() => fib(n - 2)).flatMap(x => Suspend(() => fib(n - 1)).flatMap(y => Return(x + y)))
  }

  def fib1(n: Int): IO[Int] = {
    if (n <= 1) Return(n)
    else for {
      x <- Suspend(() => fib(n - 2))
      y <- Suspend(() => fib(n - 1))
    } yield x + y
  }

  def fib2(n: Int): IO[Int] = {
    if (n <= 1) Return(n)
    else Suspend(() => fib(n - 2)).flatMap(x => Suspend(() => fib(n - 1)).map(x + _))
  }

  run(fib(25).map(_.toString).flatMap(printLine))
  run(fib1(25).map(_.toString).flatMap(printLine))
  run(fib2(25).map(_.toString).flatMap(printLine))

}
