package stress12.test

import java.io._

import org.junit.runner._
import org.scalatest.junit._

import org.sireum.kiasan.state._
import org.sireum.pilar.ast._
import org.sireum.pilar.state._
import org.sireum.util._
import org.sireum.test.framework.pilar.eval._
import org.sireum.test.kiasan.eval._
import org.sireum.topi._
import stress12._

@RunWith(classOf[JUnitRunner])
class MyIntExtensionExpTest
    extends StressTest[BasicKiasanState]
    with ExpEvaluatorTestFramework[BasicKiasanState, (BasicKiasanState, Value), ISeq[(BasicKiasanState, Value)]] {

  type S = BasicKiasanState

  val state = BasicKiasanState()

  import org.sireum.pilar.state.State.UriAccess._
  
  def newExpEvaluator(s : S) =
    KiasanEvaluatorTestUtil.newEvaluator(
      MyIntExtension,
      MyVariableAccessExtension)

  Case("alt").
    Evaluating expression "1" gives "1" satisfying { rs : ISeq[(S, V)] =>
      rs.foreach(_.value is 1)
    }

  import language.implicitConversions
  
  implicit def rf2erf(f : ((S, V)) => Unit) =
    { rs : ISeq[(S, V)] => rs.foreach { f(_) } }

  Evaluating expression "1" gives "1" satisfying { r : (S, V) => r.value is 1 }

  Evaluating.expression("2 + 3").gives("5").satisfying({ r : (S, V) => r.value is 5 })

  Evaluating expression "2 * @@x" on (state("@@x" -> 3)) gives "6" satisfying { r : (S, V) => r.value is 6 }

  val alpha = KI(0)
  val beta = KI(1)
  val gamma = KI(2)

  val rewriter = Rewriter.build[PilarAstNode]({
    case NameExp(NameUser("alpha")) => ValueExp(alpha)
    case NameExp(NameUser("beta"))  => ValueExp(beta)
    case NameExp(NameUser("gamma")) => ValueExp(gamma)
    case LiteralExp(_, n : Int, _)  => ValueExp(CI(n))
  })

  override def expRewriter(exp : Exp) : Exp = rewriter(exp).asInstanceOf[Exp]

  Evaluating expression "alpha * 2" on (state()) gives "beta, where beta=alpha*2" satisfying { r : (S, V) =>
    r.value is beta
    r.state.pathConditions is "beta == alpha * 2"
  }

  Evaluating expression "@@y * 3 + 2" on (state("@@y" -> alpha)) gives
    "gamma, where gamma=beta+2 and beta=alpha*3" satisfying { r : (S, V) =>
      r.value is gamma
      r.state.pathConditions is ("beta == alpha * 3", "gamma == beta + 2")
      check(r.state) is TopiResult.SAT
      checkModel(r.state)
    }

  def checkModel(s : S) {
    val m = getModel(s)
    val r = Rewriter.build[Exp]({
      case ValueExp(ki : KI) => ValueExp(m(ki))
    })
    val evaluator = newExpEvaluator(state)
    for (pc <- s.pathConditions)
      evaluator.evalExp(state, r(pc)).foreach(_.value is 1)
  }

  // BEGIN TODO
  Evaluating expression "- -1" gives "1" satisfying { r : (S, V) => r.value is 1 }

  Evaluating expression "-(-2 * @@x)" on (state("@@x" -> 3)) gives "-6" satisfying { r : (S, V) => r.value is 6 }

  Evaluating expression "-(alpha * 2)" on (state()) gives "gamma, where gamma=-beta and beta=alpha*2" satisfying { r : (S, V) =>
    r.value is gamma
    r.state.pathConditions is ("beta == alpha * 2", "gamma == -beta")
    check(r.state) is TopiResult.SAT
    checkModel(r.state)
  }
  // END TODO
}