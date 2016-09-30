import cats._, data._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

// Works
object Example {
  type Stack = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
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

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Works
object Example3 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx5[Foo, Bar, Writer[Int, ?], Writer[String, ?], State[Int, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .evalState(0)
      .runWriter
      .runWriter
      .run
}

// Works
object Example4 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx6[Foo, Bar, Writer[Int, ?], Writer[String, ?], State[Int, ?], State[String, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program.into[Stack2]))
      .evalState(0)
      .evalState("hello")
      .runWriter
      .runWriter
      .run
}

// Works
object Example5 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx4[Foo, Bar, State[Int, ?], State[String, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
  } yield v

  val x =
    BarInterpreter.runBarState(FooInterpreter.runFooState(program.into[Stack2]))
      .evalState(0)
      .evalState("hello")
      .run
}

// Works
object Example6 {
  type Stack = Fx.fx2[Foo, Bar]
  type Stack2 = Fx.fx6[Foo, Bar, Writer[Int, ?], Writer[String, ?], State[Int, ?], State[String, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.fooId[Stack]("a")
    v <- Bar.barId[Stack](3)
  } yield v

  val x =
    BarInterpreter.runBarWriterState(FooInterpreter.runFooWriterState(program.into[Stack2]))
      .evalState(0)
      .evalState("hello")
      .runWriter
      .runWriter
      .run
}

// Works
object ExampleP {
  type Stack = Fx.fx4[Foo, Bar, Writer[Int, ?], Writer[String, ?]]

  val program: Eff[Stack, String] = for {
    _ <- Foo.program[Stack]
    v <- Bar.program[Stack]
  } yield v

  val x =
    BarInterpreter.runBar(FooInterpreter.runFoo(program))
      .runWriter
      .runWriter
      .run
}
