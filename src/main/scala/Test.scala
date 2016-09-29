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

  val x: Eff[Stack2, Option[String]] = program.into[Stack2]
  val a: ((Option[String], List[String]), List[Int])  = KVStoreInterpreter.runKVStore(KVStoreInterpreter2.runKVStore(x))
    .evalState[Map[String, String]](Map.empty[String, String])
    .evalState[Map[String, Int]](Map.empty[String, Int])
    .runWriter[String]
    .runWriter[Int]
    .run
}
