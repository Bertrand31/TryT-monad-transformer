import cats._
import cats.implicits._
import cats.data.OptionT
import org.scalatest.FlatSpec
import scala.util.{Failure, Success, Try}
import tryt.TryT

class TryTSpec extends FlatSpec {

  import org.scalatest.Matchers._

  behavior of "The TryT monad transformer"

  val sample: TryT[Option, Int] = TryT.pure[Option, Int](3)

  "The map method" should "replace the inner value" in {

    val mapped = sample.map(_.toString)
    assert(mapped.value === Some(Success("3")))
  }

  "The transform method" should "apply a function to the inner Try" in {

    val transformed = sample.transform(inner => Try { assert(inner.isFailure) })
    transformed.value.get.foreach(throwable => {
      assert(throwable.toString == "org.scalatest.exceptions.TestFailedException: inner.isFailure was false")
    })
  }

  "The flatTransform method" should "replace the outer Option" in {

    val transformed = sample.flatTransform(_ => None)
    assert(transformed.value === None)
  }

  "The flatTransformT method" should "flatMap over the inner Try" in {

    val transformed = sample.flatTransformT(_ => TryT[Option, Int](None))
    assert(transformed.value === None)
  }

  "The flatMap method" should "flatMap over both monads" in {

    val transformed = sample.flatMap(inner => Some(Success(inner + 1)))
    assert(transformed.value === Some(Success(4)))
  }

  "The flatMapT method" should "flatMap over both monads" in {

    val transformed = sample.flatMapT(inner => TryT[Option, Int](Some(Success(inner + 1))))
    assert(transformed.value === Some(Success(4)))
  }

  "The subflatMap method" should "flatMap over the inner Try" in {

    val transformed = sample.subflatMap(inner => Success(inner + 1))
    assert(transformed.value === Some(Success(4)))
  }

  "The fold method" should "fold the inner Try" in {

    val transformed = sample.fold(_ => new Error("foo"), _ + 1)
    assert(transformed === Some(4))

    val sampleFailure = TryT[Option, Int](Some(Failure(new Error("bar"))))
    val transformedFailure = sampleFailure.fold(_ => new Error("foo"), _ + 1)
    assert(transformedFailure.get.toString == "java.lang.Error: foo")
  }

  "The foldF method" should "fold the inner Try" in {

    val transformed = sample.foldF(_ => Some(new Error("foo")), int => Some(int + 1))
    assert(transformed === Some(4))
  }

  "The bimap method" should "replace either side" in {

    val transformed = sample.bimap(err => new Error(err.toString + "bar"), _ + 1)
    assert(transformed.value === Some(Success(4)))

    val sampleFailure = TryT[Option, Int](Some(Failure(new Error("bar"))))
    val transformedFailure = sampleFailure.bimap(err => new Error(err.toString + " foo"), _ + 1)
    val thrown = intercept[Error] {
      transformedFailure.value.get.get
    }
    assert(thrown.toString == "java.lang.Error: java.lang.Error: bar foo")
  }

  "The filter method" should "assert a predicate on the inner value" in {

    val transformed = sample.filter(_ > 4)
    val thrown = intercept[Exception] {
      transformed.value.get.get
    }
    assert(thrown.toString == "java.util.NoSuchElementException: Predicate does not hold for 3")
  }

  "The collect method" should "collect the inner value" in {

    val transformed = sample.collect({
      case x: Int if x < 5 => x + 1
    })
    assert(transformed.value.get.get == 4)
  }

  "The isSuccess method" should "apply isSuccess to the inner Try" in {

    val transformed = sample.isSuccess
    assert(transformed.get == true)
  }

  "The isFailure method" should "apply isFailure to the inner Try" in {

    val transformed = sample.isFailure
    assert(transformed.get == false)
  }

  "The toOption method" should "make an OptionT" in {

    val transformed = sample.toOption
    assert(transformed == OptionT[Option, Int](Some(Some(3))))
  }

  "The getOrElse method" should "apply getOrElse to the inner Try" in {

    val transformed = sample.getOrElse(5)
    assert(transformed.get == 3)
  }

  "The getOrElseF method" should "apply getOrElse to the inner Try" in {

    val transformed = sample.getOrElseF(Some(5))
    assert(transformed.get == 3)
  }

  "The valueOr method" should "get the value or use the fallback function" in {

    val transformed = sample.valueOr(_ => 5)
    assert(transformed.get == 3)
  }

  "The valueOrF method" should "get the value or use the fallback function" in {

    val transformed = sample.valueOrF(_ => Some(5))
    assert(transformed.get == 3)
  }

  "The forall method" should "apply a predicate to the inner value" in {

    assert(sample.forall(_ > 3) == Some(false))
    assert(sample.forall(_ == 3) == Some(true))
  }

  "The exists method" should "apply a predicate to the inner value" in {

    assert(sample.exists(_ > 3) == Some(false))
    assert(sample.exists(_ == 3) == Some(true))
  }

  "The pure companion object method" should "build a TryT with Some(Success(input))" in {

    assert(TryT.pure[Option, Int](3).value == Some(Success(3)))
  }
}
