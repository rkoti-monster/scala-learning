package io.example.one

import io.example.one.ZioExample1.put
import zio.{RIO, Task, ZIO}

import scala.io.StdIn

case class UserID(name: String)

case class UserProfile(name: String)

object Database {
  trait Service {
    def lookup(id: UserID): Task[UserProfile]
    def update(id: UserID, profile: UserProfile): Task[Unit]
  }
}

trait Database {
  def database: Database.Service
}

object db {
  def lookup(id: UserID): RIO[Database, UserProfile] =
    ZIO.accessM(_.database.lookup(id))

  def update(id: UserID, profile: UserProfile): RIO[Database, Unit] =
    ZIO.accessM(_.database.update(id, profile))
}


trait DatabaseLive extends Database {
  def database: Database.Service =
    new Database.Service {
      def lookup(id: UserID): Task[UserProfile] = ???
      def update(id: UserID, profile: UserProfile): Task[Unit] = ???
    }
}

object DatabaseLive extends DatabaseLive

class TestService extends Database.Service {
  private var map: Map[UserID, UserProfile] = Map(UserID("rkoti") -> UserProfile("rkoti_profile"))

  def setTestData(map0: Map[UserID, UserProfile]): Task[Unit] =
    Task {
      map = map0
    }

  def getTestData: Task[Map[UserID, UserProfile]] =
    Task(map)

  def lookup(id: UserID): Task[UserProfile] =
    Task(map(id))

  def update(id: UserID, profile: UserProfile): Task[Unit] =
    Task.effect {
      map = map + (id -> profile)
    }
}


trait TestDatabase extends Database {
  val database: TestService = new TestService
}

object TestDatabase extends TestDatabase

object Sample extends App {
  val read = ZIO.effect(StdIn.readLine())
  def main: RIO[Database, Unit] = ???

  def main2: Task[Unit] =
    main.provide(DatabaseLive)

  def code: RIO[Database, Unit] =
    for {
      _    <- put("Hello! What is your name?")
      name <- read
      profile  <- db.lookup(UserID(name))
      _ <- put(profile.name)
    } yield ()


  def code2: Task[Unit] =
    code.provide(TestDatabase)

  val runtime = zio.Runtime.default
   runtime.unsafeRun(code2)
}
