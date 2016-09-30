import cats._
import cats.data._
import cats.implicits._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

sealed trait StringKV[+A]

case class StringGet(key: String) extends StringKV[String]

object StringKV {
  type _stringKV[R] = StringKV |= R

  /** get returns a T value if the key exists */
  def get[R :_stringKV](key: String): Eff[R, String] =
    Eff.send[StringKV, R, String](StringGet(key))

  def program[R : _stringKV]: Eff[R, String] = for {
    _ <- get[R]("a")
    _ <- get[R]("2")
  } yield "a"
}

object StringKVInterpreter {
  type _writerString[R] = Writer[String, ?] |= R

  def runStringKVWriter[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[StringKV, R, U], writer:_writerString[U]): Eff[U, A] = {
    translate(effects)(new Translate[StringKV, U] {
      def apply[X](kv: StringKV[X]): Eff[U, X] =
        kv match {
          case StringGet(key) =>
            for {
              _ <- tell("a")
            } yield "a"
        }
    })
  }
}
