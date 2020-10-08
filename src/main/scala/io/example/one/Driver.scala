package io.example.one

import scala.io.StdIn.readLine

object Driver extends App {

  sealed trait IO[A] {
    self =>
    def run: A

    def map[B](f: A => B): IO[B] = new IO[B] {
      def run: B = f(self.run)
    }

    def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {
      def run: B = f(self.run).run
    }
  }

  object IO {
    def unit[A](a: => A): IO[A] = new IO[A] {
      def run: A = a
    }

    def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa flatMap f

    def apply[A](a: => A): IO[A] = unit(a)
  }

  def printLine[T](str: T): IO[Unit] = {
    IO {
      println(str)
    }
  }

  def readLn: IO[String] = {
    IO {
      readLine
    }
  }

  def forever[A, B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
    a flatMap (_ => t)
  }


  // readLn.flatMap(printLine).run
  // readLn.map(_.toUpperCase()).flatMap(printLine).run
  val intScan = readLn.map(_.toInt).flatMap(printLine)
  intScan.run

  forever(printLine("This is repeating")).run

}
