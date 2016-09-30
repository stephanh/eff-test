import cats._, data._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// Works
object Example {
  type Stack = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack]("b")
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program))
      .runWriter
      .runWriter
      .run
}

// Works
object Example2 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack]("b")
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Works
object ExampleP {
  type Stack = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    _ <- Foo.program[Stack]
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program))
      .runWriter
      .runWriter
      .run
}
 // Doesn't work
object ExampleP2 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    _ <- Foo.program[Stack]
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Works
object ExampleP3 {
  type Stack = Fx.fx1[Bar]
  type Stack2 = Fx.fx2[Bar, Writer[Int, ?]]

  val program: Eff[Stack, Int] = for {
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(program.into[Stack2])
      .runWriter
      .run
}
