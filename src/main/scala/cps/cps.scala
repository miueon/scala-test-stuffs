package cps
// article addr: https://chshersh.com/cps

import cats.effect.IO
import cats.syntax.all.*

case class UserSession(id: String)
case class UserId(id: String)
case class Email(email: String)
case class Id(id: String)

def validateUserSession(us: UserSession): IO[Option[UserId]] = ???
def getEmailByUserId(uid: UserId): IO[Option[Email]] = ???
def getUserIdByEmail(emai: Email): IO[Option[UserId]] = ???
def insertUserEmail(uid: UserId, email: Email): IO[Either[InternalDbError, Id]] = ???

enum InternalDbError:
  case ConnectionError
  case QueryError

enum AppError:
  case UserSessionIsInvalid
  case UserAlreadyHasEmail
  case UserHasDifferentEmail
  case EmailIsTaken
  case DbError(e: InternalDbError)

trait AssociateEmail:
  def associateEmail(us: UserSession, email: Email): IO[Either[AppError, Id]]

object BaseLine:
  given AssociateEmail with
    def associateEmail(us: UserSession, email: Email): IO[Either[AppError, Id]] =
      validateUserSession(us).flatMap {
        case None => Left(AppError.UserSessionIsInvalid).pure
        case Some(userId) =>
          getEmailByUserId(userId).flatMap {
            case Some(otherEmail) =>
              if otherEmail == email then Left(AppError.UserAlreadyHasEmail).pure
              else Left(AppError.UserHasDifferentEmail).pure
            case None =>
              getUserIdByEmail(email).flatMap {
                case Some(otherUserId) => Left(AppError.EmailIsTaken).pure
                case None =>
                  insertUserEmail(userId, email).flatMap {
                    case Left(e)   => Left(AppError.DbError(e)).pure
                    case Right(id) => Right(id).pure
                  }
              }
          }
      }
  end given
end BaseLine

object CPSTransformation:
  def withUserSession[A](us: UserSession, next: (UserId => IO[Either[AppError, A]])): IO[Either[AppError, A]] =
    validateUserSession(us).flatMap {
      case None      => Left(AppError.UserSessionIsInvalid).pure
      case Some(uid) => next(uid)
    }

  def withCheckedUserMail(uid: UserId, email: Email, next: => IO[Either[AppError, Id]]): IO[Either[AppError, Id]] =
    getEmailByUserId(uid).flatMap {
      case Some(otherEmail) =>
        if otherEmail == email then Left(AppError.UserAlreadyHasEmail).pure
        else Left(AppError.UserHasDifferentEmail).pure
      case None => next
    }

  def withCheckedOtherUserEmail(email: Email, next: => IO[Either[AppError, Id]]): IO[Either[AppError, Id]] =
    getUserIdByEmail(email).flatMap {
      case Some(_) => Left(AppError.EmailIsTaken).pure
      case None    => next
    }

  def withEmailInsert[A](uid: UserId, email: Email, next: (Id => IO[Either[AppError, A]])): IO[Either[AppError, A]] =
    insertUserEmail(uid, email).flatMap {
      case Left(e)   => Left(AppError.DbError(e)).pure
      case Right(id) => next(id)
    }

  given AssociateEmail with
    def associateEmail(us: UserSession, email: Email): IO[Either[AppError, Id]] =
      withUserSession(
        us,
        uid =>
          withCheckedUserMail(
            uid,
            email,
            withCheckedOtherUserEmail(email, withEmailInsert(uid, email, id => Right(id).pure))
          )
      )
end CPSTransformation
