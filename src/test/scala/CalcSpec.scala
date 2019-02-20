import org.scalatest.time.SpanSugar._
import org.scalatest.{FlatSpec, DiagrammedAssertions}
import org.scalatest.concurrent.Timeouts
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._

class CalcSpec extends FlatSpec with DiagrammedAssertions with Timeouts with MockitoSugar {

  val calc = new Calc

  "sum関数" should "整数の配列を取得し、それらを足しあわせた整数を返すことができる" in {
    assert(calc.sum(Seq(1, 2, 3)) === 6)
    assert(calc.sum(Seq(0)) === 0)
    assert(calc.sum(Seq(-1, 1)) === 0)
    assert(calc.sum(Seq()) === 0)
  }

  it should "Intの最大を上回った際にはオーバーフローする" in {
    assert(calc.sum(Seq(Integer.MAX_VALUE, 1)) === Integer.MIN_VALUE)
  }

  "div関数" should "整数を2つ受け取り、分子を分母で割った浮動小数点の値を返す" in {
    assert(calc.div(6, 3) === 2.0)
    assert(calc.div(1,3) === 0.33333333333333333)
  }

  it should "0で割ろうとした際には実行時例外が投げられる" in {
    intercept[ArithmeticException] {
      calc.div(1,0)
    }
  }

  "isPrime関数" should "その値が素数であるかどうかのプール値を返す" in {
    assert(calc.isPrime(0) === false)
    assert(calc.isPrime(-1) === false)
    assert(calc.isPrime(2))
    assert(calc.isPrime(17))
  }

  it should "100万以下の値の素数判定を1秒以内で処理できる" in {
    failAfter(1000 millis) {
      assert(calc.isPrime(9999991))
    }
  }

  "Calcのモックオブジェクト" should "振る舞いを偽装することができる" in {
    val mockCalc = mock[Calc]
    when(mockCalc.sum(Seq(3, 4, 5))).thenReturn(12)
    assert(mockCalc.sum(Seq(3, 4, 5)) === 12)
  }
}