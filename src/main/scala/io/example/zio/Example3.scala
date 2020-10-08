package io.example.zio

import zio.{Has, RIO, ZIO, ZLayer}

object Example3 extends App {

  //////////////////////////// Module ////////////////////////
  // Service
  trait UserRepo {
    def get(id: Int): RIO[Any, User]
  }

  // Implementation(s)
  object UserRepoImpl extends UserRepo {
    override def get(id: Int): RIO[Any, User] = ZIO.succeed(User(2, "X", "Y"))
  }

  // Accessor(s)
  def getUserRepoAccessor(id: Int): RIO[Has[UserRepo], User] = RIO.accessM(_.get.get(id))

  // Layer(s)
  def userRepoLive: ZLayer[Any, Nothing, Has[UserRepo]] = ZLayer.succeed(UserRepoImpl)
  ////////////////////////////////////////////////////


  //////////////////////////// Module ////////////////////////
  // Service
  trait PermissionRepo {
    def isPermitted(user: User): RIO[Any, Boolean]
  }

  // Implementation(s)
  object PermissionRepoImpl extends PermissionRepo {
    override def isPermitted(user: User): RIO[Any, Boolean] = ZIO.succeed(true)
  }

  // Accessor(s)
  def isPermittedRepoAccessor(user: User): RIO[Has[PermissionRepo], Boolean] = RIO.accessM(_.get.isPermitted(user))

  // Layer(s)
  def permissionRepoLive: ZLayer[Any, Nothing, Has[PermissionRepo]] = ZLayer.succeed(PermissionRepoImpl)
  ////////////////////////////////////////////////////


  //////////////////////////// Module ////////////////////////
  // Service
  trait UserService {
    def get(id: Int): RIO[Any, User]
  }

  // Implementation(s)
  class UserServiceImpl(userRepo: UserRepo) extends UserService {
    override def get(id: Int): RIO[Any, User] = userRepo.get(id)
  }

  // Accessor(s)
  def getUserServiceAccessor(id: Int): RIO[Has[UserService], User] = RIO.accessM(_.get.get(id))

  // Layer(s)
  def userServiceLive: ZLayer[Has[UserRepo], Nothing, Has[UserService]] = ZLayer.fromService {
    userRepo =>
      new UserServiceImpl(userRepo)
  }
  ////////////////////////////////////////////////////

  //////////////////////////// Module ////////////////////////
  // Service
  trait PermissionService {
    def isPermitted(user: User): RIO[Any, Boolean]
  }

  // Implementation(s)
  class PermissionServiceImpl(permissionRepo: PermissionRepo) extends PermissionService {
    override def isPermitted(user: User): RIO[Any, Boolean] = permissionRepo.isPermitted(user)
  }

  // Accessor(s)
  def isPermittedServiceAccessor(user: User): RIO[Has[PermissionService], Boolean] = RIO.accessM(_.get.isPermitted(user))

  // Layer(s)
  def permissionServiceLive: ZLayer[Has[PermissionRepo], Nothing, Has[PermissionService]] = ZLayer.fromFunction {
    permissionRepo => {
      new PermissionServiceImpl(permissionRepo.get)
    }
  }
  ////////////////////////////////////////////////////


  //////////////////////////// Module ////////////////////////
  // Service
  trait SecureUserService {
    def get(id: Int, requester: User): RIO[Any, User]
  }

  // Implementation(s)
  class SecureUserServiceImpl(permissionService: PermissionService, userService: UserService) extends SecureUserService {
    override def get(id: Int, requester: User): RIO[Any, User] =
        for {
          permitted <- permissionService.isPermitted(requester)
          user <- userService.get(id) if permitted
        } yield user
  }

  // Accessor(s)
  def getSecureAccessor(id: Int, requester: User): RIO[Has[SecureUserService], User] = RIO.accessM(_.get.get(id, requester))

  // Layer(s)
  def secureUserServiceLive: ZLayer[Has[UserService] with Has[PermissionService], Nothing, Has[SecureUserService]] =
    ZLayer.fromServices[UserService, PermissionService, SecureUserService] {
      (userService, permissionService) => new SecureUserServiceImpl(permissionService, userService)
  }
  ////////////////////////////////////////////////////

  // Dependency Injection
  val appEnvironment = ( (userRepoLive >>> userServiceLive) ++ (permissionRepoLive >>> permissionServiceLive ) ) >>> secureUserServiceLive

  println(zio.Runtime.default.unsafeRun(getSecureAccessor(1, User(1, "", "")).provideLayer(appEnvironment)))
}
