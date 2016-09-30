import cats._, data._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._


// Doesn't work
object KVExample {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx6[KVStore, KVStore2, State[Map[String, String], ?], State[Map[String, Int], ?], Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  val x =
    KVStoreInterpreter.runKVStore(KVStoreInterpreter2.runKVStore(program.into[Stack2]))
      .evalState(Map.empty[String, String])
      .evalState(Map.empty[String, Int])
      .runWriter
      .runWriter
      .run
}

// Works
object KVExample2 {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx6[KVStore, KVStore2, State[Map[String, String], ?], State[Map[String, Int], ?], Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.get[Stack]("aa")
    y <- KVStore2.get[Stack]("bb")
  } yield y

  val x =
    KVStoreInterpreter.runKVStore(KVStoreInterpreter2.runKVStore(program.into[Stack2]))
      .evalState(Map.empty[String, String])
      .evalState(Map.empty[String, Int])
      .runWriter
      .runWriter
      .run

}

// Doesn't Work
object KVExample3 {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx4[KVStore, KVStore2,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  val x =
    KVStoreInterpreter.runKVStoreWriter(KVStoreInterpreter2.runKVStoreWriter(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Doesn't work
object KVExample4 {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx4[KVStore, KVStore2, State[Map[String, String], ?], State[Map[String, Int], ?]]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  val x =
    KVStoreInterpreter.runKVStoreState(KVStoreInterpreter2.runKVStoreState(program.into[Stack2]))
      .evalState(Map.empty[String, Int])
      .evalState(Map.empty[String, String])
      .run
}

// Doesn't Work
object KVExample5 {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx4[KVStore, KVStore2,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, String] = for {
    x <- KVStore.program2[Stack]
    y <- KVStore2.program2[Stack]
  } yield y

  val x =
    KVStoreInterpreter.runKVStoreWriter(KVStoreInterpreter2.runKVStoreWriter(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Doesn't Work
object KVExample6 {
  type Stack = Fx.fx2[KVStore, KVStore2]
  type Stack2 = Fx.fx4[KVStore, KVStore2,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, String] = for {
    x <- KVStore.program3[Stack]
    y <- KVStore2.program3[Stack]
  } yield y

  val x =
    KVStoreInterpreter.runKVStoreWriter(KVStoreInterpreter2.runKVStoreWriter(program.into[Stack2]))
      .runWriter
      .runWriter
      .run
}

// Doesn't Work
object KVExample7 {
  type Stack = Fx.fx2[IntKV, StringKV]
  type Stack2 = Fx.fx4[IntKV, StringKV,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, String] = for {
    y <- StringKV.program[Stack]
  } yield y

  val x =
    IntKVInterpreter.runIntKVWriter(StringKVInterpreter.runStringKVWriter(program.into[Stack2]))
      .runWriter[String]
      .runWriter[Int]
      .run
}

// Works
object KVExample7a {
  type Stack = Fx.fx2[IntKV, StringKV]
  type Stack2 = Fx.fx4[IntKV, StringKV,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, String] = for {
    y <- StringKV.get[Stack]("a")
  } yield y

  val x =
    IntKVInterpreter.runIntKVWriter(StringKVInterpreter.runStringKVWriter(program.into[Stack2]))
      .runWriter[String]
      .runWriter[Int]
      .run
}


// Works
object KVExample8 {
  type Stack = Fx.fx1[StringKV]
  type Stack2 = Fx.fx2[StringKV,  Writer[String, ?]]

  val program: Eff[Stack, String] = for {
    y <- StringKV.program[Stack]
  } yield y

  val x =
    StringKVInterpreter.runStringKVWriter(program.into[Stack2])
      .runWriter
      .run
}

// Works
object KVExample9 {
  type Stack = Fx.fx1[IntKV]
  type Stack2 = Fx.fx2[IntKV,  Writer[Int, ?]]

  val program: Eff[Stack, Int] = for {
    y <- IntKV.program[Stack]
  } yield y

  val x =
    IntKVInterpreter.runIntKVWriter(program.into[Stack2])
      .runWriter
      .run
}

// Works
object KVExample10 {
  type Stack = Fx.fx2[IntKV, Foo]
  type Stack2 = Fx.fx4[IntKV, Foo,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, Int] = for {
    y <- IntKV.program[Stack]
  } yield y

  val x =
    IntKVInterpreter.runIntKVWriter(FooInterpreter.runFoo(program.into[Stack2]))
      .runWriter[String]
      .runWriter[Int]
      .run
}

// Works
object KVExample11 {
  type Stack = Fx.fx2[IntKV, Bar]
  type Stack2 = Fx.fx4[IntKV, Bar,  Writer[Int, ?], Writer[Int, ?]]

  val program: Eff[Stack, Int] = for {
    y <- IntKV.program[Stack]
  } yield y

  val x =
    IntKVInterpreter.runIntKVWriter(BarInterpreter.runBar(program.into[Stack2]))
      .runWriter[Int]
      .runWriter[Int]
      .run
}

// Works
object KVExample12 {
  type Stack = Fx.fx2[StringKV, Bar]
  type Stack2 = Fx.fx4[StringKV, Bar,  Writer[String, ?], Writer[Int, ?]]

  val program: Eff[Stack, String] = for {
    y <- StringKV.program[Stack]
  } yield y

  val x =
    StringKVInterpreter.runStringKVWriter(BarInterpreter.runBar(program.into[Stack2]))
      .runWriter[String]
      .runWriter[Int]
      .run
}

