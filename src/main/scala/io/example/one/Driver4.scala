package io.example.one

object Driver4 {

  sealed trait Async[A] {
    def flatMap[B](f: A => Async[B]): Async[B] =
      FlatMap(this, f)

    def map[B](f: A => B): Async[B] =
      flatMap(x => Return(f(x)))
  }

  case class Return[A](a: A) extends Async[A]

  //case class Suspend[A](resume: Par[A]) extends Async[A]
  case class FlatMap[A, B](sub: Async[A], k: A => Async[B]) extends Async[B]


}
