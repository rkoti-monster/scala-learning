package io.example.one

object Driver5 extends App {

  def flip(a: List[Option[Int]]): Option[List[Int]] = {
    a.foldLeft[Option[List[Int]]](Option(List())) {
      (acc, option) => acc.flatMap(a => option.map(v => a ++ List(v)))
    }
  }

  println(flip(List(Option(1), None, Option(2))))
}
