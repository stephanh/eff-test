import cats._, data._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._

sealed trait Bar[+A]
case class BarId(v: String) extends Bar[Int]

object Bar {
  type _bar[R] = Bar |= R

  def barId[R : _bar](v: String): Eff[R, Int] =
    Eff.send[Bar, R, Int](BarId(v))

  def program[R: _bar]: Eff[R, Int] = for {
    x <- barId[R]("a")
    y <- barId[R]("b")
  } yield x
}

object BarInterpreter {
  type _writerInt[R] = Writer[Int, ?] |= R

  def runBar[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Bar, R, U], writer:_writerInt[U]): Eff[U, A] = {
    translate(effects)(new Translate[Bar, U] {
      def apply[X](bar: Bar[X]): Eff[U, X] =
        bar match {
          case BarId(v) => for {
            _ <- tell(-10)
          } yield 10
        }
    })
  }
}
