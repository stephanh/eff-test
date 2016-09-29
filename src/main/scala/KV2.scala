import cats._
import cats.data._
import cats.implicits._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._


sealed trait KVStore2[+A]

case class Put2[T](key: String, value: String) extends KVStore2[Unit]
case class Get2[T](key: String) extends KVStore2[Option[T]]
case class Delete2(key: String) extends KVStore2[Unit]

object KVStore2 {
  type _kvstore2[R] = KVStore2 |= R

  /** put returns nothing (i.e. Unit) */
  def put[R :_kvstore2](key: String, value: String): Eff[R, Unit] =
    Eff.send[KVStore2, R, Unit](Put2(key, value))

  /** get returns a T value if the key exists */
  def get[R :_kvstore2](key: String): Eff[R, Option[String]] =
    Eff.send[KVStore2, R, Option[String]](Get2(key))

  /** delete returns nothing (i.e. Unit) */
  def delete[R :_kvstore2](key: String): Eff[R, Unit] =
    Eff.send(Delete2(key))

  /** update composes get and set, and returns nothing. */
  def update[R :_kvstore2](key: String, f: String => String): Eff[R, Unit] =
    get[R](key).map(_.map(f)).void

  def program[R :_kvstore2]: Eff[R, Option[String]] =
    for {
      _ <- put("wild-cats", "hello")
      _ <- update[R]("wild-cats", _ + "bar")
      _ <- put("tame-cats", "bye")
      n <- get[R]("wild-cats")
      _ <- delete("tame-cats")
    } yield n
}

object KVStoreInterpreter2 {
  type _writerString[R] = Writer[String, ?] |= R
  type _stateMap[R]     = State[Map[String, String], ?] |= R

  def runKVStore[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[KVStore2, R, U],
      writer:_writerString[U],
      state:_stateMap[U]): Eff[U, A] = {

    translate(effects)(new Translate[KVStore2, U] {
      def apply[X](kv: KVStore2[X]): Eff[U, X] =
        kv match {
          case Put2(key, value) =>
            for {
              _ <- tell(key)
              _ <- modify((map: Map[String, String]) => map.updated(key, value))
            } yield ()

          case Get2(key) =>
            for {
              _ <- tell(key)
              m <- get[U, Map[String, String]]
            } yield m(key).asInstanceOf[X]

          case Delete2(key) =>
            for {
              _ <- tell(key)
              u <- modify((map: Map[String, String]) => map - key)
            } yield ()
        }
    })
  }
}
