package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._
import scala.math.sqrt

import TweetLength.MaxTweetLength
import Polynomial._
import Calculator._

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("compute delta is accurate"){
    var sigA = Var(1.0)
    var sigB = Var(1.0)
    var sigC = Var(1.0)

    assert(Polynomial.computeDelta(sigA,sigB,sigC)() == -3.0)

    sigB() = 0.0

    assert(Polynomial.computeDelta(sigA,sigB,sigC)() == -4.0)

    sigA() = 4.0
    sigC() = 2.0

    assert(Polynomial.computeDelta(sigA,sigB,sigC)() == -32.0)

  }

  test("compute a different delta"){
    var sigA: Var[Double] = new Var(1.0)
    var sigB: Var[Double] = new Var(4.0)
    var sigC: Var[Double] = new Var(1.0)
    var delta = Polynomial.computeDelta(sigA,sigB,sigC)
    var root = Polynomial.computeSolutions(sigA,sigB,sigC,delta)

    System.out.println(root())
    System.out.println(delta())

    System.out.println(root())
    System.out.println(delta())


  }

  test("computeValues"){
    var map: Map[String, Signal[Expr]] = Map()
    var a: Signal[Expr] = Signal(Plus(Ref("b"),Literal(1)))
    var b: Signal[Expr] = Signal(Ref("c"))
    var c: Signal[Expr] = Signal(Ref("a"))
    map += ("a" -> a)
    map += ("b" -> b)
    map += ("c" -> c)
    assert(Calculator.computeValues(map).size > 0)

  }

}
