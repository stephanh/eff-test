import cats._, data._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// Works
object ExampleInto {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
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
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program))
      .runWriter
      .runWriter
      .run
}
 // Doesn't work
object ExamplePInto {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Works
object ExamplePBarInto {
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

// Doesn't Work
object ExamplePIntoStaggered {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx3[Foo, Bar, Writer[String, ?]]
  type Stack3 = Fx.fx3[Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    _ <- Foo.program[Stack]
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]).into[Stack3])
      .runWriter
      .runWriter
      .run
}

// Doesn't Work
object ExamplePIntoOneWriter {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx3[Foo, Bar, Writer[String, ?]]

  val program: Eff[Stack, Int] = for {
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBarSimple(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter
      .run
}
