package calculator

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import TweetLength.MaxTweetLength

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

  /******************
    ** TWEET COLOR **
    *****************/
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

  test("colorForRemainingCharsCount with a changed signal") {
    val len = Var(100)
    val left = Var(140 - len())
    val colour = TweetLength.colorForRemainingCharsCount(left)
    def check(col: String) = {
      assert(colour() == col)  }
    check("green")
    len() = 140
    check("orange")
    len() = 150
    check("red")
  }

  /*****************
    ** POLYNOMIAL **
    ****************/
  test("computeSolutions with a changed signal") {
    val a = Var(1.0)
    val b = Var(1.0)
    val c = Var(1.0)
    def delta = Polynomial.computeDelta(a,b,c)
    val solutions = Polynomial.computeSolutions(a,b,c,delta)
    def check(sol: Set[Double]) = {
      assert(solutions() == sol)  }
    check(Set())
    b() = -5.0
    c() = 6.0
    check(Set(3.0,2.0))
    a() = 2.0
    b() = -7.0
    c() = 3.0
    check(Set(3.0,0.5))
    a() = -1
    b() = 7.0
    c() = -10.0
    check(Set(5.0,2.0))
    a() = 1
    b() = -2.0
    c() = 1.0
    check(Set(1.0))
    a() = 1
    b() = -4.0
    c() = 4.0
    check(Set(2.0))
  }

  /*****************
    ** CALCULATOR **
    ****************/
  test("computeValues with a changed signal") {
    val a:Var[Expr] = Var(Literal(1.0))
    val b:Var[Expr] = Var(Literal(1.0))
    val c:Var[Expr] = Var(Literal(1.0))
    val d:Var[Expr] = Var(Literal(1.0))
    val e:Var[Expr] = Var(Literal(1.0))
    val f:Var[Expr] = Var(Literal(1.0))
    val g:Var[Expr] = Var(Literal(1.0))
    val h:Var[Expr] = Var(Literal(1.0))
    val i:Var[Expr] = Var(Literal(1.0))
    val j:Var[Expr] = Var(Literal(1.0))
    def namedExpressions[String, Signal[Expr]] =
      Map("a"->a,
        "b"->b,
        "c"->c,
        "d"->d,
        "e"->e,
        "f"->f,
        "g"->g,
        "h"->h,
        "i"->i,
        "j"->j
      )
    val solutions = Calculator.computeValues(namedExpressions)
    def check(sol: Map[String,Signal[Double]]) = {
      for{
        (n1,e1)<-sol
      }yield assert(solutions(n1)().equals(sol(n1)()))
    }
    check(Map("a"->Signal(1.0),
      "b"->Signal(1.0),
      "c"->Signal(1.0),
      "d"->Signal(1.0),
      "e"->Signal(1.0),
      "f"->Signal(1.0),
      "g"->Signal(1.0),
      "h"->Signal(1.0),
      "i"->Signal(1.0),
      "j"->Signal(1.0)
    ))
    a() = Literal(1.0)
    b() = Ref("a")
    c() = Plus(Ref("b"),Literal(1.0))
    d() = Minus(Ref("c"),Literal(1.0))
    e() = Times(Ref("d"),Literal(2.0))
    f() = Divide(Ref("e"),Literal(2.0))
    g() = Plus(Ref("e"),Ref("f"))
    h() = Minus(Ref("f"),Ref("g"))
    i() = Times(Ref("g"),Ref("h"))
    j() = Divide(Ref("i"),Ref("h"))
    check(Map("a"->Signal(1.0),
      "b"->Signal(1.0),
      "c"->Signal(2.0),
      "d"->Signal(1.0),
      "e"->Signal(2.0),
      "f"->Signal(1.0),
      "g"->Signal(3.0),
      "h"->Signal(-2.0),
      "i"->Signal(-6.0),
      "j"->Signal(3.0)
    ))
    c() = Plus(Ref("b"),Literal(2.0))
    d() = Minus(Ref("c"),Literal(0.0))
    e() = Times(Ref("d"),Literal(3.0))
    f() = Divide(Ref("e"),Literal(3.0))
    check(Map("a"->Signal(1.0),
      "b"->Signal(1.0),
      "c"->Signal(3.0),
      "d"->Signal(3.0),
      "e"->Signal(9.0),
      "f"->Signal(3.0),
      "g"->Signal(12.0),
      "h"->Signal(-9.0),
      "i"->Signal(-108.0),
      "j"->Signal(12.0)
    ))
    a() = Ref("b")
    check(Map("a"->Signal(Double.NaN),
      "b"->Signal(Double.NaN),
      "c"->Signal(Double.NaN),
      "d"->Signal(Double.NaN),
      "e"->Signal(Double.NaN),
      "f"->Signal(Double.NaN),
      "g"->Signal(Double.NaN),
      "h"->Signal(Double.NaN),
      "i"->Signal(Double.NaN),
      "j"->Signal(Double.NaN)
    ))
    a() = Ref("b")
    b() = Ref("c")
    c() = Ref("a")
    check(Map("a"->Signal(Double.NaN),
      "b"->Signal(Double.NaN),
      "c"->Signal(Double.NaN),
      "d"->Signal(Double.NaN),
      "e"->Signal(Double.NaN),
      "f"->Signal(Double.NaN),
      "g"->Signal(Double.NaN),
      "h"->Signal(Double.NaN),
      "i"->Signal(Double.NaN),
      "j"->Signal(Double.NaN)
    ))
  }
}
