package io.example.zio

import zio.{RIO, ZIO}

object Example2 extends App {

  trait UserRepo {
    def get(id: Int): RIO[Any, User]
  }

  object UserRepoImpl extends UserRepo {
    override def get(id: Int): RIO[Any, User] = ZIO.succeed(User(2, "X", "Y"))
  }


  trait PermissionRepo {
    def isPermitted(user: User): RIO[Any, Boolean]
  }

  object PermissionRepoImpl extends PermissionRepo {
    override def isPermitted(user: User): RIO[Any, Boolean] = ZIO.succeed(true)
  }


  trait UserService {
    def get(id: Int): RIO[Any, User]
  }

  class UserServiceImpl(userRepo: UserRepo) extends UserService {
    override def get(id: Int): RIO[Any, User] = userRepo.get(id)
  }


  trait PermissionService {
    def isPermitted(user: User): RIO[Any, Boolean]
  }

  class PermissionServiceImpl(permissionRepo: PermissionRepo) extends PermissionService {
    override def isPermitted(user: User): RIO[Any, Boolean] = permissionRepo.isPermitted(user)
  }


  trait SecureUserService {
    def get(id: Int, requester: User): RIO[Any, User]
  }

  class SecureUserServiceImpl(userService: UserService, permissionService: PermissionService) extends SecureUserService {
    override def get(id: Int, requester: User): RIO[Any, User] =
      for {
        permitted <- permissionService.isPermitted(requester)
        user <- userService.get(id) if permitted
      } yield user
  }


  class UserPermissionRepoWrapper(userRepo: UserRepo, permissionRepo: PermissionRepo) extends UserRepo with PermissionRepo {
    override def get(id: Int): RIO[Any, User] = userRepo.get(id)

    override def isPermitted(user: User): RIO[Any, Boolean] = permissionRepo.isPermitted(user)
  }


  class UserPermissionServiceWrapper(userService: UserService, permissionService: PermissionService) extends UserService with PermissionService {
    override def get(id: Int): RIO[Any, User] = userService.get(id)

    override def isPermitted(user: User): RIO[Any, Boolean] = permissionService.isPermitted(user)
  }

  println(zio.Runtime.default.unsafeRun(
    new SecureUserServiceImpl(new UserServiceImpl(UserRepoImpl), new PermissionServiceImpl(PermissionRepoImpl)).get(1, User(1, "a", "b"))
  )
  )
}
