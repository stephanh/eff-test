import cats._
import cats.data._
import cats.implicits._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._


sealed trait KVStore[+A]

case class Put(key: String, value: Int) extends KVStore[Unit]
case class Get(key: String) extends KVStore[Option[Int]]
case class Delete(key: String) extends KVStore[Unit]

object KVStore {
  type _kvstore[R] = KVStore |= R

  /** put returns nothing (i.e. Unit) */
  def put[R :_kvstore](key: String, value: Int): Eff[R, Unit] =
    Eff.send[KVStore, R, Unit](Put(key, value))

  /** get returns a T value if the key exists */
  def get[R :_kvstore](key: String): Eff[R, Option[Int]] =
    Eff.send[KVStore, R, Option[Int]](Get(key))

  /** delete returns nothing (i.e. Unit) */
  def delete[R :_kvstore](key: String): Eff[R, Unit] =
    Eff.send(Delete(key))

  /** update composes get and set, and returns nothing. */
  def update[R :_kvstore](key: String, f: Int => Int): Eff[R, Unit] =
    get[R](key).map(_.map(f)).void

  def program[R :_kvstore]: Eff[R, Option[Int]] =
    for {
      _ <- put[R]("wild-cats", 2)
      _ <- update[R]("wild-cats", _ + 12)
      _ <- put[R]("tame-cats", 5)
      n <- get[R]("wild-cats")
      _ <- delete[R]("tame-cats")
    } yield n

  def program2[R : _kvstore]: Eff[R, Int] = for {
    _ <- put[R]("a", 1)
    _ <- put[R]("2", 2)
  } yield 3

  def program3[R : _kvstore]: Eff[R, Int] = for {
    _ <- get[R]("a")
    _ <- get[R]("2")
  } yield 3
}

object KVStoreInterpreter {
  type _writerInt[R] = Writer[Int, ?] |= R
  type _stateMap[R]     = State[Map[String, Int], ?] |= R

  def runKVStore[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[KVStore, R, U],
      writer:_writerInt[U],
      state:_stateMap[U]): Eff[U, A] = {

    translate(effects)(new Translate[KVStore, U] {
      def apply[X](kv: KVStore[X]): Eff[U, X] =
        kv match {
          case Put(key, value) =>
            for {
              _ <- tell(value)
              _ <- modify((map: Map[String, Int]) => map.updated(key, value))
            } yield ()

          case Get(key) =>
            for {
              _ <- tell(-1)
              m <- get[U, Map[String, Int]]
            } yield m.get(key)

          case Delete(key) =>
            for {
              _ <- tell(-2)
              u <- modify((map: Map[String, Int]) => map - key)
            } yield ()
        }
    })
  }

  def runKVStoreWriter[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[KVStore, R, U], writer:_writerInt[U]): Eff[U, A] = {
    translate(effects)(new Translate[KVStore, U] {
      def apply[X](kv: KVStore[X]): Eff[U, X] =
        kv match {
          case Put(key, value) =>
            for {
              _ <- tell(value)
            } yield ()

          case Get(key) =>
            for {
              _ <- tell(-1)
            } yield Option(3)

          case Delete(key) =>
            for {
              _ <- tell(-2)
            } yield ()
        }
    })
  }

  def runKVStoreState[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[KVStore, R, U], state:_stateMap[U]): Eff[U, A] = {

    translate(effects)(new Translate[KVStore, U] {
      def apply[X](kv: KVStore[X]): Eff[U, X] =
        kv match {
          case Put(key, value) =>
            for {
              _ <- modify((map: Map[String, Int]) => map.updated(key, value))
            } yield ()

          case Get(key) =>
            for {
              m <- get[U, Map[String, Int]]
            } yield m.get(key)

          case Delete(key) =>
            for {
              u <- modify((map: Map[String, Int]) => map - key)
            } yield ()
        }
    })
  }

}
