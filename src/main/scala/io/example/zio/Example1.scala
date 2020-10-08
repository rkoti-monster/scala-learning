// No ZLayers
package io.example.zio

import zio.{RIO, ZIO}

case class User(id: Int, firstName: String, lastName: String)

object Example1 extends App {

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
    def get(id: Int): RIO[UserRepo, User]
  }

  object UserServiceImpl extends UserService {
    override def get(id: Int): RIO[UserRepo, User] = RIO.accessM(_.get(id))
  }




  trait PermissionService {
    def isPermitted(user: User): RIO[PermissionRepo, Boolean]
  }

  object PermissionServiceImpl extends PermissionService {
    override def isPermitted(user: User): RIO[PermissionRepo, Boolean] = RIO.accessM(_.isPermitted(user))
  }




  trait SecureUserService {
    def get(id: Int, requester: User): RIO[UserService with PermissionService, User]
  }

  object SecureUserServiceImpl extends SecureUserService {
    override def get(id: Int, requester: User): RIO[UserService with PermissionService, User] =
      RIO.accessM { r => {
        for {
          permitted <- r.isPermitted(requester)
          user <- r.get(id) if permitted
        } yield user
      }.provide(new UserPermissionRepoWrapper(UserRepoImpl, PermissionRepoImpl))
      }
  }



    class UserPermissionRepoWrapper(userRepo: UserRepo, permissionRepo: PermissionRepo) extends UserRepo with PermissionRepo {
      override def get(id: Int): RIO[Any, User] = userRepo.get(id)

      override def isPermitted(user: User): RIO[Any, Boolean] = permissionRepo.isPermitted(user)
    }


    class UserPermissionServiceWrapper(userService: UserService, permissionService: PermissionService) extends UserService with PermissionService {
      override def get(id: Int): RIO[UserRepo, User] = userService.get(id)

      override def isPermitted(user: User): RIO[PermissionRepo, Boolean] = permissionService.isPermitted(user)
    }



    println(zio.Runtime.default.unsafeRun(
      SecureUserServiceImpl.get(1, User(1, "a", "b")
      ).provide(new UserPermissionServiceWrapper(UserServiceImpl, PermissionServiceImpl)
      )
    ))
  }

