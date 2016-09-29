import cats._, data._

import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Test {
  type Stack = Fx.fx3[KVStore, State[Map[String, Int], ?], Writer[Int, ?]]

  val (result, logs) =
    KVStoreInterpreter.runKVStore(KVStore.program[Stack]).evalState(Map.empty[String, Int]).runWriter.run
}

object Test2 {
  type Stack = Fx.fx2[KVStore, KVStore2]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  type Stack2 = Fx.fx6[KVStore, KVStore2, State[Map[String, String], ?], State[Map[String, Int], ?], Writer[String, ?], Writer[Int, ?]]

  type Stack3 = Fx.fx5[KVStore, State[Map[String, String], ?], State[Map[String, Int], ?], Writer[String, ?], Writer[Int, ?]]
  type Stack4 = Fx.fx4[State[Map[String, String], ?], State[Map[String, Int], ?], Writer[String, ?], Writer[Int, ?]]


  val x: Eff[Stack2, Option[String]] = program.into[Stack2]
  // val a: ((Option[String], List[String]), List[Int]) =
  //   KVStoreInterpreter.runKVStore[Stack3, Stack4, Option[String]](KVStoreInterpreter2.runKVStore[Stack2, Stack3, Option[String]](x))
  //     .evalState[Map[String, String]](Map.empty[String, String])
  //     .evalState[Map[String, Int]](Map.empty[String, Int])
  //     .runWriter[String]
  //     .runWriter[Int]
  //     .run
}

object Test3 {
  type Stack = Fx.fx2[KVStore, KVStore2]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  type Stack2 = Fx.fx4[KVStore, KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack3 = Fx.fx3[KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack4 = Fx.fx4[State[Map[String, Int], ?], Writer[Int, ?], State[Map[String, String], ?], Writer[String, ?]]

  val a: Eff[Stack2, Option[String]] = program.into[Stack2]
  val b: Eff[Stack3, Option[String]] = KVStoreInterpreter.runKVStore[Stack2, Stack3, Option[String]](a)
  //val c: Eff[Stack4, Option[String]] = KVStoreInterpreter2.runKVStore[Stack3, Stack4, Option[String]](b)
}

object Test4 {
  type Stack = Fx.fx2[KVStore, KVStore2]

  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  type Stack2 = Fx.fx4[KVStore, KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack3 = Fx.fx3[KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack4 = Fx.fx5[KVStore2, State[Map[String, Int], ?], Writer[Int, ?], State[Map[String, String], ?], Writer[String, ?]]
  type Stack5 = Fx.fx4[State[Map[String, Int], ?], Writer[Int, ?], State[Map[String, String], ?], Writer[String, ?]]

  val a: Eff[Stack2, Option[String]] = program.into[Stack2]
  val b: Eff[Stack3, Option[String]] = KVStoreInterpreter.runKVStore[Stack2, Stack3, Option[String]](a)
  val c: Eff[Stack4, Option[String]] = b.into[Stack4]
  val d: Eff[Stack5, Option[String]] = KVStoreInterpreter2.runKVStore[Stack4, Stack5, Option[String]](c)
  val x =
    d
      .evalState[Map[String, String]](Map.empty[String, String])
      .evalState[Map[String, Int]](Map.empty[String, Int])
      .runWriter[String]
  //    .runWriter[Int]
  //     .run
}

/** Same as Test4 but with runWriter[Int] uncommented. */
object Test5 {
  type Stack = Fx.fx2[KVStore, KVStore2]

  // Runtime error
  val program: Eff[Stack, Option[String]] = for {
    x <- KVStore.program[Stack]
    y <- KVStore2.program[Stack]
  } yield y

  type Stack2 = Fx.fx4[KVStore, KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack3 = Fx.fx3[KVStore2, State[Map[String, Int], ?], Writer[Int, ?]]
  type Stack4 = Fx.fx5[KVStore2, State[Map[String, Int], ?], Writer[Int, ?], State[Map[String, String], ?], Writer[String, ?]]
  type Stack5 = Fx.fx4[State[Map[String, Int], ?], Writer[Int, ?], State[Map[String, String], ?], Writer[String, ?]]

  val a: Eff[Stack2, Option[String]] = program.into[Stack2]
  val b: Eff[Stack3, Option[String]] = KVStoreInterpreter.runKVStore[Stack2, Stack3, Option[String]](a)
  val c: Eff[Stack4, Option[String]] = b.into[Stack4]
  val d: Eff[Stack5, Option[String]] = KVStoreInterpreter2.runKVStore[Stack4, Stack5, Option[String]](c)
  val x =
    d
      .evalState[Map[String, String]](Map.empty[String, String])
      .evalState[Map[String, Int]](Map.empty[String, Int])
      .runWriter[String]
      .runWriter[Int]
  //     .run
}
