import cats._
import cats.data._
import cats.implicits._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

sealed trait IntKV[+A]

case class IntGet(key: String) extends IntKV[Int]

object IntKV {
  type _intKV[R] = IntKV |= R

  /** get returns a T value if the key exists */
  def get2[R :_intKV](key: String): Eff[R, Int] =
    Eff.send[IntKV, R, Int](IntGet(key))

  def program[R : _intKV]: Eff[R, Int] = for {
    _ <- get2[R]("a")
    _ <- get2[R]("2")
  } yield 3
}

object IntKVInterpreter {
  type _writerInt[R] = Writer[Int, ?] |= R

  def runIntKVWriter[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[IntKV, R, U], writer:_writerInt[U]): Eff[U, A] = {
    translate(effects)(new Translate[IntKV, U] {
      def apply[X](kv: IntKV[X]): Eff[U, X] =
        kv match {
          case IntGet(key) =>
            for {
              _ <- tell(-1)
            } yield 3
        }
    })
  }
}
