package stress12.test

import org.junit.runner._
import org.scalatest.junit._

import org.sireum.extension._
import org.sireum.kiasan.state._
import org.sireum.pilar.ast._
import org.sireum.pilar.eval._
import org.sireum.pilar.state._
import org.sireum.util._
import org.sireum.test.framework.pilar.eval._
import org.sireum.test.kiasan.eval._
import org.sireum.topi._

import stress12._

@RunWith(classOf[JUnitRunner])
class MyIntListExtensionExpTest
    extends StressTest[KiasanStateWithHeap]
    with ExpEvaluatorTestFramework[KiasanStateWithHeap, (KiasanStateWithHeap, Value), ISeq[(KiasanStateWithHeap, Value)]]
    with ActionEvaluatorTestFramework[KiasanStateWithHeap, KiasanStateWithHeap, ISeq[KiasanStateWithHeap]] {

  type S = KiasanStateWithHeap

  val config =
    KiasanEvaluatorTestUtil.newConfig[S](
      MyIntListExtension,
      MyIntExtension,
      UriValueExtension,
      MyVariableAccessExtension)

  val heapConfig = {
    import EvaluatorHeapConfig._
    config.heapEvalConfig
  }
  val lhid = heapConfig.heapId(MyIntListExtension.listHeapIdKey)

  val state = KiasanStateWithHeap(Vector(Vector()))

  def newExpEvaluator(s : S) = config.evaluator.mainEvaluator

  import language.implicitConversions

  implicit def rf2erf(f : ((S, V)) => Unit) =
    { rs : ISeq[(S, V)] => rs.foreach { f(_) } }

  implicit def se2sr(f : S => Unit) = { sr : ISeq[S] => sr.map(f) }

  Evaluating expression "`[1, 2, 3]" on state gives "[1, 2, 3]" satisfying { r : (S, V) =>
    println(r.state)
    println(r.value)
  }

  def newActionEvaluator(s : S) = config.evaluator.mainEvaluator

  import MyIntListExtension._

  Evaluating action "update(@@y, 0);" on {
    val (s, rv) = state.newObject(lhid, intDataFieldUri -> CI(5), nextFieldUri -> NilValue)
    s.variable("@@y", rv)
  } gives "a new state" satisfying { s : S =>
    val rv = s.variable("@@y").asInstanceOf[ReferenceValue]
    s.lookup[Int](rv, intDataFieldUri) is 0
    s.lookup[Value](rv, nextFieldUri) is NilValue
  }

  Evaluating action "update(@@y, 0);" on {
    val (s, rv) = state.newObject(lhid)
    s.variable("@@y", rv)
  } gives "a new state" satisfying { s : S =>
    val rv = s.variable("@@y").asInstanceOf[ReferenceValue]
    s.lookup[Int](rv, intDataFieldUri) is 0
    s ? (rv, nextFieldUri) is false
  }

  {
    val (s, rv) = {
      val (s0, v) = state.newObject(lhid, intDataFieldUri -> CI(5), nextFieldUri -> NilValue)
      (s0.variable("@@y", v), v)
    }
    Evaluating expression "car @@y" on s gives "5" satisfying { r : (S, V) =>
      r.value is 5
      r.state is s
    }
  }

  val alpha = KI(1)

  Evaluating expression "car @@y" on {
    val (s, rv) = state.newObject(lhid)
    s.variable("@@y", rv)
  } gives "alpha" satisfying { r : (S, V) =>
    val (s, rv) = (r.state, r.state.variable("@@y").asInstanceOf[ReferenceValue])
    s.lookup[Value](rv, intDataFieldUri) is alpha
    s ? (rv, nextFieldUri) is false
    r.value is alpha
  }

  {
    val (s, rv) = {
      val (s0, v) = state.newObject(lhid, intDataFieldUri -> CI(5), nextFieldUri -> NilValue)
      (s0.variable("@@y", v), v)
    }
    Evaluating expression "cdr @@y" on s gives "nil" satisfying { r : (S, V) =>
      r.value is NilValue
      r.state is s
    }
  }

  Evaluating expression "cdr @@y" on {
    val (s, rv) = state.newObject(lhid)
    s.variable("@@y", rv)
  } gives "nil,node" satisfying { r : (S, V) =>
    val (s, rv) = (r.state, r.state.variable("@@y").asInstanceOf[ReferenceValue])
    s ? (rv, intDataFieldUri) is false
    r.value match {
      case NilValue => s.lookup[Value](rv, nextFieldUri) is NilValue
      case rv2 : ReferenceValue =>
        s.lookup[Value](rv, nextFieldUri) is rv2
        s ? (rv2, nextFieldUri) is false
        s ? (rv2, intDataFieldUri) is false
    }
  }

  val beta = KI(1)
  val gamma = KI(2)

  val rewriter = Rewriter.build[PilarAstNode]({
    case NameExp(NameUser("alpha")) => ValueExp(alpha)
    case NameExp(NameUser("beta"))  => ValueExp(beta)
    case NameExp(NameUser("gamma")) => ValueExp(gamma)
    case LiteralExp(_, n : Int, _)  => ValueExp(CI(n))
  })

  override def expRewriter(exp : Exp) : Exp = rewriter(exp).asInstanceOf[Exp]

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
  Evaluating expression "1 + `[1, 2]" on state gives "[1, 1, 2]" satisfying { r : (S, V) =>
    println(r.state)
    println(r.value)
  }

  Evaluating expression "getElementAt(`[1, 2], 0)" on state gives "1" satisfying { r : (S, V) =>
    r.value is 1
  }

  Evaluating expression "getElementAt(`[1, 2], 1)" on state gives "2" satisfying { r : (S, V) =>
    r.value is 2
  }

  Evaluating expression "getElementAt(`[1, 2], 2)" on state gives "1" satisfying { r : (S, V) =>
    r.value is 2
  }
  // END TODO
}