import cats._, data._

import org.atnos.eff._
import org.atnos.eff.interpret._
import org.atnos.eff.all._

sealed trait Foo[+A]
case class FooId(v: String) extends Foo[String]

object Foo {
  type _foo[R] = Foo |= R

  def fooId[R : _foo](v: String): Eff[R, String] =
    Eff.send[Foo, R, String](FooId(v))

  def program[R: _foo]: Eff[R, String] = for {
    x <- fooId[R]("hello")
    y <- fooId[R]("world")
  } yield x
}

object FooInterpreter {
  type _writerString[R] = Writer[String, ?] |= R

  def runFoo[R, U, A](effects: Eff[R, A])
    (implicit m: Member.Aux[Foo, R, U], writer:_writerString[U]): Eff[U, A] = {
    translate(effects)(new Translate[Foo, U] {
      def apply[X](foo: Foo[X]): Eff[U, X] =
        foo match {
          case FooId(v) => for {
            _ <- tell(v)
          } yield v
        }
    })
  }
}
