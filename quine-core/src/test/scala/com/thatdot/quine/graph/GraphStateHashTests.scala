package com.thatdot.quine.graph

import akka.actor.ActorSystem
import akka.pattern.Patterns
import akka.stream.Materializer
import akka.util.Timeout
import com.thatdot.quine.model.{QuineId, QuineValue}
import com.thatdot.quine.persistor.{EventEffectOrder, InMemoryPersistor, PersistenceAgent}
import org.scalacheck.Gen
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AsyncFunSuite
import org.scalacheck.rng.Seed

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration.DurationInt

class GraphStateHashTests extends AsyncFunSuite with BeforeAndAfterAll {

  // Override this if tests need to be skipped
  def runnable: Boolean = true

  def makePersistor(system: ActorSystem): PersistenceAgent = InMemoryPersistor.empty

  implicit val timeout: Timeout = Timeout(10.seconds)
  val idProvider: QuineIdLongProvider = QuineIdLongProvider()
  implicit val graph: LiteralOpsGraph = Await.result(
    GraphService(
      "historical-query-tests",
      effectOrder = EventEffectOrder.PersistorFirst,
      persistor = makePersistor,
      idProvider = idProvider
    ),
    timeout.duration
  )
  implicit val ec: ExecutionContextExecutor = graph.system.dispatcher
  implicit val materializer: Materializer = graph.materializer

  val qid42: QuineId = idProvider.customIdToQid(42L) // meaning of life
  val qid30: QuineId = idProvider.customIdToQid(30L) // MIU-producible number

  val genSetProperty: Gen[Future[Unit]] = for {
    node <- Gen.oneOf(qid42, qid30)
    key <- Gen.oneOf("prop1", "prop2", "prop3")
    value <- Gen.choose(min = 1L, max = 100L)
  } yield graph.literalOps.setProp(node = node, key = key, value = QuineValue.Integer(value))

  val genSetLabel: Gen[Future[Unit]] = for {
    node <- Gen.oneOf(qid42, qid30)
    label <- Gen.oneOf("label1", "label2", "label3", "label4")
  } yield graph.literalOps.setLabel(node = node, label = label)

  val genSetProperties: Gen[List[Future[Unit]]] = Gen.containerOf[List, Future[Unit]](genSetProperty)
  val setPropertyParameters: Gen.Parameters = Gen.Parameters.default.withSize(1000)

  val genSetLabels: Gen[List[Future[Unit]]] = Gen.containerOf[List, Future[Unit]](genSetLabel)
  val setLabelParameters: Gen.Parameters = Gen.Parameters.default.withSize(100)

  val seed: Seed = Seed(42L)

  def generate: Future[List[Unit]] = Future.sequence {
    genSetProperties(setPropertyParameters, seed).get ++ genSetLabels(setLabelParameters, seed).get
  }

  override def beforeAll(): Unit = {
    Await.result(
      for {
        _ <- Patterns.retry(
          () => Future(graph.requiredGraphIsReady()),
          attempts = 100,
          delay = 200.millis,
          graph.system.scheduler,
          graph.system.dispatcher
        )
        _ <- generate
      } yield (),
      timeout.duration * 2L
    )
  }

  override def afterAll(): Unit =
    Await.result(graph.shutdown(), timeout.duration * 2L)

  test("a sequence of property/value sets produces a consistent graph state as measured by NodeSummary") {
    assume(runnable)
    val parallelism = 4

    val expected = NodeSummary(
      count = 2,
      ids = -1950346221,
      labels = 1644734498,
      properties = Map(
        'prop1 -> 734222525,
        'prop2 -> 1842478326,
        'prop3 -> -1771739217
      ))

    graph
      .enumerateAllNodeIds()
      .mapAsyncUnordered(parallelism) { node =>
        graph.literalOps
          .getPropsAndLabels(node)
          .map { case (properties, labels) => NodeSummary(node, properties, labels) }
      }
      .runFold(NodeSummary.zero)(NodeSummary.combine)
      .map { propertySummary =>
        assert(propertySummary == expected)
      }
  }
}
