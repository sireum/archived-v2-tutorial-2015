package stress12

import org.sireum.extension._
import org.sireum.extension.annotation._
import org.sireum.pilar.ast._
import org.sireum.pilar.eval._
import org.sireum.pilar.state._
import org.sireum.kiasan.extension.KiasanExtension._
import org.sireum.kiasan.extension.annotation._
import org.sireum.kiasan.state._
import org.sireum.util._
import org.sireum.topi.process._
import java.io._
import org.sireum.topi.annotation._

object MyIntExtension extends ExtensionCompanion {
  def create[S <: KiasanStatePart[S]](
    config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]]) =
    new MyIntExtension(config)

  @BackEnd(value = "Z3", mode = "Process")
  def z3BackEndPart = new TopiProcess.BackEndPart {
    def expTranslator(sb : StringBuilder) = {
      val lineSep = System.getProperty("line.separator")

        @inline
        implicit def i2string(i : Int) = i.toString

        @inline
        def println(ss : String*) {
          for (s <- ss)
            sb.append(s)
          sb.append(lineSep)
        }

      val numSet = new java.util.BitSet

        def declareConst(num : Int) {
          if (!numSet.get(num)) {
            numSet.set(num)
            println("(declare-const i!", num, " Int)")
          }
        }

        def bin(freshNum : Int, n : String, m : String, op : String) {
          declareConst(freshNum)
          println("(assert (= i!", freshNum, " (", op, " ", n, " ", m, ")))")
        }

        def sbin(v1 : String, v2 : String, op : String) {
          println("(assert (", op, " ", v1, " ", v2, "))")
        }

        def nsbin(v1 : String, v2 : String, op : String) {
          println("(assert (not (", op, " ", v1, " ", v2, ")))")
        }

        def i2s : Exp --> String = {
          case e : LiteralExp =>
            e.literal.toString
          case ValueExp(CI(n)) =>
            if (n < 0)
              "(- " + -n.toLong + ")"
            else
              n.toString
          case ValueExp(KI(num)) =>
            declareConst(num)
            "i!" + num
        }

        // BEGIN TODO
        def unMinus(freshNum : Int, n : String) {
          declareConst(freshNum)
          println("(assert (= i!", freshNum, " (- ", n, ")))")
        }
      // END TODO

      {
        case BinaryExp("==", ValueExp(KI(freshNum)), BinaryExp(op, n, m)) // 
        if (i2s isDefinedAt n) && (i2s isDefinedAt m) && (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") =>
          op match {
            case "+" => bin(freshNum, i2s(n), i2s(m), "+")
            case "-" => bin(freshNum, i2s(n), i2s(m), "-")
            case "*" => bin(freshNum, i2s(n), i2s(m), "*")
            case "/" => bin(freshNum, i2s(n), i2s(m), "div")
            case "%" => bin(freshNum, i2s(n), i2s(m), "rem")
          }
        case BinaryExp(op, n, m) if (i2s isDefinedAt n) && (i2s isDefinedAt m) &&
          (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") =>
          op match {
            case "==" => sbin(i2s(n), i2s(m), "=")
            case "!=" => nsbin(i2s(n), i2s(m), "=")
            case ">"  => sbin(i2s(n), i2s(m), ">")
            case ">=" => sbin(i2s(n), i2s(m), ">=")
            case "<"  => sbin(i2s(n), i2s(m), "<")
            case "<=" => sbin(i2s(n), i2s(m), "<=")
          }
        // BEGIN TODO
        case BinaryExp("==", ValueExp(KI(freshNum)), UnaryExp("-", v)) =>
          unMinus(freshNum, i2s(v))
        // END TODO
      }
    }

    def stateRewriter(m : IMap[String, Value]) = {
      case v @ KI(num) => m.getOrElse("ii!" + num, v)
    }
  }

  val URI_PATH = "stress12/MyIntExtension"
  val CINT_TYPE_URI = "pilar://typeext/" + URI_PATH + "/CInt"
  val KINT_TYPE_URI = "pilar://typeext/" + URI_PATH + "/KInt"
}

sealed abstract class I extends NonReferenceValue
// concrete integer
case class CI(value : Int) extends I with ConcreteValue {
  def typeUri = MyIntExtension.CINT_TYPE_URI
}
// symbolic integer
case class KI(num : Int) extends I with KiasanValue {
  def typeUri = MyIntExtension.KINT_TYPE_URI
}

final class MyIntExtension[S <: KiasanStatePart[S]](
  config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]])
    extends Extension[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]] {

  import MyIntExtension._

  def uriPath = URI_PATH

  implicit def re2r(p : (S, Value)) = ilist(p)

  @Literal(classOf[Int])
  def literal : (S, Int) --> ISeq[(S, Value)] = {
    case (s, n) => (s, CI(n))
  }

  @DefaultValue
  def defValue : (S, ResourceUri) --> ISeq[(S, Value)] = {
    case (s, IntegerExtension.Type) => (s, CI(0))
  }

  @FreshKiasanValue
  def freshKI : (S, ResourceUri) --> (S, Value) = {
    case (s, KINT_TYPE_URI) => {
      val (nextS, num) = s.next(KINT_TYPE_URI)
      (nextS, KI(num))
    }
  }

  @Binaries(Array("+", "-", "*", "/", "%"))
  def opAEval : (S, Value, String, Value) --> ISeq[(S, Value)] = {
    case (s, c : CI, opA : String, d : CI) => (s, opASem(opA)(c, d))
    case (s, v : CI, opA : String, w : KI) => opAHelper(s, v, opA, w)
    case (s, v : KI, opA : String, w : CI) => opAHelper(s, v, opA, w)
    case (s, v : KI, opA : String, w : KI) => opAHelper(s, v, opA, w)
  }

  def opASem(opA : String) : (CI, CI) => CI = { (c, d) =>
    opA match {
      case "+" => CI(c.value + d.value)
      case "-" => CI(c.value - d.value)
      case "*" => CI(c.value * d.value)
      case "/" => CI(c.value / d.value)
      case "%" => CI(c.value % d.value)
    }
  }

  implicit def v2e(v : Value) : Exp = ValueExp(v)

  def opAHelper(s : S, v : Value, opA : String, w : Value) : ISeq[(S, Value)] = {
    val (nextS, a) = freshKI(s, KINT_TYPE_URI)
    (nextS.addPathCondition(BinaryExp("==", a, BinaryExp(opA, v, w))), a)
  }

  @Binaries(Array("==", "!=", ">", ">=", "<", "<="))
  def opREval : (S, Value, String, Value) --> ISeq[(S, Value)] = {
    case (s, c : CI, opR : String, d : CI) => (s, opRSem(opR)(c, d))
    case (s, v : CI, opR : String, w : KI) => opRHelper(s, v, opR, w)
    case (s, v : KI, opR : String, w : CI) => opRHelper(s, v, opR, w)
    case (s, v : KI, opR : String, w : KI) => opRHelper(s, v, opR, w)
  }

  def opRSem(opR : String) : (CI, CI) => CI = { (c, d) =>
    if (opR match {
      case "==" => c.value == d.value
      case "!=" => c.value != d.value
      case ">"  => c.value > d.value
      case ">=" => c.value >= d.value
      case "<"  => c.value < d.value
      case "<=" => c.value <= d.value
    }) CI(1) else CI(0)
  }

  val comp = Map("==" -> "!=", "!=" -> "==", ">" -> "<=", ">=" -> "<", "<" -> ">=", "<=" -> ">=")

  def opRHelper(s : S, v : Value, opR : String, w : Value) : ISeq[(S, Value)] = {
    ilist(
      (s.addPathCondition(BinaryExp(opR, v, w)).requestInconsistencyCheck, CI(1)),
      (s.addPathCondition(BinaryExp(comp(opR), v, w)).requestInconsistencyCheck, CI(0)))
  }

  // BEGIN TODO
  @Unary("-")
  def unaryMinusEval : (S, Value) --> ISeq[(S, Value)] = {
    case (s, c : CI) =>
      (s, CI(-c.value))
    case (s, alpha : KI) =>
      val (nextS, beta) = freshKI(s, KINT_TYPE_URI)
      (nextS.addPathCondition(BinaryExp("==", beta, UnaryExp("-", alpha))), beta)
  }
  // END TODO
}