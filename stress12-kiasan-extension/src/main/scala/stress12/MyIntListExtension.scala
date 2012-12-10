package stress12

import org.sireum.extension._
import org.sireum.extension.annotation._
import org.sireum.kiasan.extension._
import org.sireum.kiasan.state._
import org.sireum.pilar.eval._
import org.sireum.pilar.state._
import org.sireum.util._

object MyIntListExtension extends ExtensionCompanion {
  def create[S <: KiasanStatePart[S] with Heap[S]](
    config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]]) =
    new MyIntListExtension(config)

  val URI_PATH = "stress12/MyIntListExtension"
  val KINT_LIST_TYPE_URI = "pilar://typeext/" + URI_PATH + "/KIntList"

  val nextFieldUri = "next"
  val intDataFieldUri = "data"
  val listHeapIdKey = "ListHeapId"
}

object NilValue extends ConcreteValue with ReferenceValue

final class MyIntListExtension[S <: KiasanStatePart[S] with Heap[S]](
  config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]])
    extends Extension[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]] {

  import MyIntListExtension._

  def uriPath = URI_PATH
  
  import language.implicitConversions

  implicit def re2r(p : (S, Value)) = ilist(p)

  val heapConfig = config.adapter[EvaluatorHeapConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]]]
  val lhid = heapConfig.heapId(listHeapIdKey)

  @NewList
  def newList : (S, ISeq[Value]) --> ISeq[(S, Value)] = {
    case (s, vs) if vs.forall { _.isInstanceOf[I] } =>
      var l : ReferenceValue = NilValue
      var newS = s
      for (v <- vs.reverse) {
        val p = newS.newObject(lhid, nextFieldUri -> l, intDataFieldUri -> v)
        newS = first2(p)
        l = second2(p)
      }
      (newS, l)
  }

  implicit def s2sr(s : S) = ilist(s)

  @TopLevel @ActionExt
  def update : (S, Value, Value) --> ISeq[S] = {
    case (s, rv @ Heap.RV(hid, _), v : I) if hid == lhid =>
      s.update(rv, intDataFieldUri, v)
  }

  val sei = config.semanticsExtension.adapter[KiasanSemanticsExtensionConsumer[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]]]

  @TopLevel @ExpExt
  def car : (S, Value) --> ISeq[(S, Value)] = {
    case (s, rv @ Heap.RV(hid, _)) if hid == lhid =>
      if (s.hasFieldValue(rv, intDataFieldUri))
        (s, s.lookup(rv, intDataFieldUri))
      else {
        val (s2, alpha) = sei.freshKiasanValue(s, MyIntExtension.KINT_TYPE_URI)
        val s3 = s2.update(rv, intDataFieldUri, alpha)
        (s3, alpha)
      }
  }

  @TopLevel @ExpExt
  def cdr : (S, Value) --> ISeq[(S, Value)] = {
    case (s, rv @ Heap.RV(hid, _)) if hid == lhid =>
      if (s.hasFieldValue(rv, nextFieldUri))
        (s, s.lookup(rv, nextFieldUri))
      else {
        val (newS, rv2) = s.newObject(lhid)
        ilist((s.update(rv, nextFieldUri, NilValue), NilValue),
          (newS.update(rv, nextFieldUri, rv2), rv2))
      }
  }

  // BEGIN TODO
  @Binary("+")
  def plus : (S, Value, Value) --> ISeq[(S, Value)] = {
    case (s, i : I, rv @ Heap.RV(hid, _)) if hid == lhid =>
      s.newObject(lhid, nextFieldUri -> rv, intDataFieldUri -> i)
  }

  @TopLevel @ExpExt
  def getElementAt : (S, Value, Value) --> ISeq[(S, Value)] = {
    case (s, rv @ Heap.RV(hid, _), CI(value)) if hid == lhid =>
      val (sNodes, sErrorNodes) = getNode(s, rv, value)
      assert (sErrorNodes.isEmpty)
      val result = sNodes.flatMap(car)
      assert (!result.isEmpty)
      result
    case (s, rv @ Heap.RV(hid, _), KI(num)) if hid == lhid =>
      ilistEmpty // ??
  }

  def getNode(s : S, rv : ReferenceValue, index : Int) = {
    var i = index
    var result : ISeq[(S, Value)] = ilist((s, rv))
    var errorResult : ISeq[(S, Value)] = ilistEmpty
    while (i > 0) {
      var cdrs = result.flatMap(cdr)
      result = cdrs.filter(_._2 != NilValue)
      errorResult ++= cdrs.filter(_._2 == NilValue)
      i -= 1
    }
    (result, errorResult)
  }
  // END TODO
}