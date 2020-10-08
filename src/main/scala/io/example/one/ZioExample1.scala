package io.example.one

import zio.{Fiber, Task, UIO, URIO, ZIO}

import scala.concurrent.Future
import scala.io.StdIn

object ZioExample1 extends App {
  val s1 = ZIO.succeed(42)
  val s2 = Task.succeed(42)
  val s3 = ZIO.effectTotal(System.currentTimeMillis())
  val s4 = ZIO.fromFunction((i: Int) => i * i)
  lazy val f1 = Future.successful("Hello!")
  val zFuture: Task[String] =
    ZIO.fromFuture {
      implicit ec =>
        f1.map(_ => "Goodbye!")
    }
  val read = ZIO.effect(StdIn.readLine())

  def put(output: String) = ZIO.effectTotal(println(output))

  val program =
    for {
      _ <- put("Hello! What is your name?")
      name <- read
      _ <- put(s"Hello, ${name}, welcome to ZIO!")
    } yield ()

  def fib(n: Long): UIO[Long] = UIO {
    if (n <= 1) {
      put(s"$n HURRAY ") *>
        UIO.succeed(n)
    }
    else {
      put(s"$n >> ") *>
        fib(n - 1).zipWith(fib(n - 2))(_ + _)
    }
  }.flatten

  val fib100Fiber: UIO[Fiber[Nothing, Long]] =
    for {
      fiber <- fib(14).fork
      - <- put("joining fiber")
    } yield fiber

  for {
    env <- ZIO.environment[Int]
    _ <- put(s"The value of the environment is: $env")
  } yield env


  final case class Config(server: String, port: Int)

  val configString: URIO[Config, String] =
    for {
      server <- ZIO.access[Config](_.server)
      port <- ZIO.access[Config](_.port)
    } yield s"Server: $server, port: $port"

  trait DatabaseOps {
    def getTableNames: Task[List[String]]

    def getColumnNames(table: String): Task[List[String]]
  }

  val tablesAndColumns: ZIO[DatabaseOps, Throwable, (List[String], List[String])] =
    for {
      tables <- ZIO.accessM[DatabaseOps](_.getTableNames)
      columns <- ZIO.accessM[DatabaseOps](_.getColumnNames("user_table"))
    } yield (tables, columns)

  val square: URIO[Int, Int] =
    for {
      env <- ZIO.environment[Int]
    } yield env * env

  val result: UIO[Int] = square.provide(42)

  val mapped = fib100Fiber.map(_.join)
  val flatMapped = fib100Fiber.flatMap(_.join)

  //zio.Runtime.default.unsafeRun(result)
  // println(zio.Runtime.default.unsafeRun(fib100Fiber.map(_.join)))
  //println(zio.Runtime.default.unsafeRun(fib100Fiber))
  // println(zio.Runtime.default.unsafeRun(fib100Fiber.flatMap(_.join)))
  //println(zio.Runtime.default.unsafeRun(fib(15)))
  //println(zio.Runtime.default.unsafeRun(fib(15).map(x => put(x.toString))) *> )

  // val test = Option[Option[Int]](Some(1))
  // println(test.flatten)
  0
  /*val flatMapped1 = fib(14).flatMap(x => Task.succeed(x))
  println(zio.Runtime.default.unsafeRun(flatMapped1))

  val mapped1 = fib(14).map(x => x * 3)
  println(zio.Runtime.default.unsafeRun(mapped1))*/

  val mapped2 = fib(14).map(x => Task.succeed(x))
  //println(zio.Runtime.default.unsafeRun(zio.Runtime.default.unsafeRun(mapped2)))
  //println(zio.Runtime.default.unsafeRunAsync(ZIO.succeed("Async!")) {x: Exit[_, _] => {
  // println("This is one")
  //  println(x)
  //} })

  val s = ZIO.succeed(123).flatMap { n =>
    ZIO.succeed(println(s"NOT RECOMMENDED! $n"))
  }
}
