package io.example.one

object Driver2 extends App {
  val f = (x: Int) => x
  val g = List.fill(100000)(f).foldLeft(f)(_ compose _)
  val g1 = List.fill(1000000)(1).foldRight(0)(_ + _)
  println(g(1))
}
