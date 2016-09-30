import cats._, data._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._

sealed trait Bar[+A]
case class BarId(v: Int) extends Bar[String]

object Bar {
  type _bar[R] = Bar |= R

  def barId[R : _bar](v: Int): Eff[R, String] =
    Eff.send[Bar, R, String](BarId(v))

  def program[R: _bar]: Eff[R, String] = for {
    x <- barId[R](1)
    y <- barId[R](2)
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
            _ <- tell(v)
          } yield v.toString
        }
    })
  }

  def runBarSimple[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Bar, R, U]): Eff[U, A] = {
    translate(effects)(new Translate[Bar, U] {
      def apply[X](bar: Bar[X]): Eff[U, X] =
        bar match {
          case BarId(v) => pure(v.toString)
        }
    })
  }

  type _stateInt[R] = State[Int, ?] |= R
  def runBarState[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Bar, R, U], state:_stateInt[U]): Eff[U, A] = {
    translate(effects)(new Translate[Bar, U] {
      def apply[X](bar: Bar[X]): Eff[U, X] =
        bar match {
          case BarId(v) =>for {
            _ <- put(v)
          } yield v.toString
        }
    })
  }

  def runBarWriterState[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Bar, R, U], state: _stateInt[U], writer: _writerInt[U]): Eff[U, A] = {
    translate(effects)(new Translate[Bar, U] {
      def apply[X](bar: Bar[X]): Eff[U, X] =
        bar match {
          case BarId(v) =>for {
            _ <- tell(v)
            _ <- put(v)
          } yield v.toString
        }
    })
  }
}
