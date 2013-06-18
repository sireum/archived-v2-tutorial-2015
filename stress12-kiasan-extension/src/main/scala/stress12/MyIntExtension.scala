package stress12

import org.sireum.extension._
import org.sireum.pilar.ast._
import org.sireum.pilar.eval._
import org.sireum.pilar.state._
import org.sireum.kiasan.extension._
import org.sireum.kiasan.extension.KiasanExtension._
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

    class Context(var tc : TopiProcess.TypeCounters,
                  val sb : StringBuilder = new StringBuilder) {
      def result = (tc, sb.toString)
    }

    val lineSep = System.getProperty("line.separator")

    import language.implicitConversions
    
    @inline
    implicit def i2s(i : Int) = i.toString

    @inline
    def println(ss : String*)(implicit ctx : Context) {
      for (s <- ss)
        ctx.sb.append(s)
      ctx.sb.append(lineSep)
    }

    @inline
    def declareConst(num : Int)(implicit ctx : Context) {
      val lastNum = ctx.tc.getOrElse(KINT_TYPE_URI, -1)
      if (num > lastNum) {
        for (i <- lastNum + 1 to num)
          println("(declare-const i!", i, " Int)")
        ctx.tc = ctx.tc + (KINT_TYPE_URI -> num)
      }
    }

    @inline
    def declareEConst(e : Exp)(implicit ctx : Context) {
      e match {
        case ValueExp(KI(num)) => declareConst(num)
        case _                 =>
      }
    }

    @inline
    def un(freshNum : Int, n : String)(implicit ctx : Context) {
      declareConst(freshNum)
      println("(assert (= i!", freshNum, " (- ", n, ")))")
    }

    @inline
    def bin(freshNum : Int, n : String,
            m : String, op : String)(implicit ctx : Context) {
      declareConst(freshNum)
      println("(assert (= i!", freshNum, " (", op, " ", n, " ", m, ")))")
    }

    @inline
    def sbin(v1 : String, v2 : String, op : String)(implicit ctx : Context) = {
      println("(assert (", op, " ", v1, " ", v2, "))")
    }

    @inline
    def nsbin(v1 : String, v2 : String, op : String)(implicit ctx : Context) = {
      println("(assert (not (", op, " ", v1, " ", v2, ")))")
    }

    @inline
    implicit def v2s : Exp --> String = {
      case e : LiteralExp =>
        e.literal.toString
      case ValueExp(c : CI) =>
        val n = c.value
        if (n < 0)
          "(- " + -n.toLong + ")"
        else
          n.toString
      case ValueExp(KI(num)) =>
        "i!" + num
    }

    def expTranslator = {
      case (tc, BinaryExp("==", ValueExp(KI(freshNum)), BinaryExp(op, n, m))) //
      if (v2s isDefinedAt n) && (v2s isDefinedAt m) && (op == "+" || op == "-" || op == "*" || op == "/" || op == "%") =>
        implicit val ctx = new Context(tc)
        declareEConst(n)
        declareEConst(m)
        op match {
          case "+" => bin(freshNum, n, m, "+")
          case "-" => bin(freshNum, n, m, "-")
          case "*" => bin(freshNum, n, m, "*")
          case "/" => bin(freshNum, n, m, "div")
          case "%" => bin(freshNum, n, m, "rem")
        }
        ctx.result

      case (tc, BinaryExp(op, n, m)) if (v2s isDefinedAt n) && (v2s isDefinedAt m) &&
        (op == "==" || op == "!=" || op == "<" || op == "<=" || op == ">" || op == ">=") =>
        implicit val ctx = new Context(tc)
        declareEConst(n)
        declareEConst(m)
        op match {
          case "==" => sbin(n, m, "=")
          case "!=" => nsbin(n, m, "=")
          case ">"  => sbin(n, m, ">")
          case ">=" => sbin(n, m, ">=")
          case "<"  => sbin(n, m, "<")
          case "<=" => sbin(n, m, "<=")
        }
        ctx.result

      case (tc, BinaryExp("==", ValueExp(KI(freshNum)), UnaryExp("-", n))) =>
        implicit val ctx = new Context(tc)
        declareEConst(n)
        un(freshNum, n)
        ctx.result
    }

    def stateRewriter(m : IMap[String, Value]) = {
      case v @ KI(num) => m.getOrElse("i!" + num, v)
    }
  }

  val URI_PATH = "stress12/MyIntExtension"
  val CINT_TYPE_URI = "pilar://typeext/" + URI_PATH + "/CInt"
  val KINT_TYPE_URI = "pilar://typeext/" + URI_PATH + "/KInt"
}

sealed abstract class I extends ScalarValue

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
  
  import language.implicitConversions

  implicit def re2r(p : (S, Value)) = ilist(p)

  @Literal(classOf[Int])
  def literal : (S, Int) --> (S, Value) = {
    case (s, n) => (s, CI(n))
  }

  @DefaultValueProvider
  def defValue : (S, ResourceUri) --> ISeq[(S, Value)] = {
    case (s, IntegerExtension.Type) => (s, CI(0))
  }

  @FreshKiasanValueProvider
  def freshKI : (S, ResourceUri) --> (S, Value) = {
    case (s, KINT_TYPE_URI) => {
      val (nextS, num) = s.fresh(KINT_TYPE_URI)
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

  val comp = Map("==" -> "!=", "!=" -> "==", ">" -> "<=", ">=" -> "<", "<" -> ">=", "<=" -> ">")

  def opRHelper(s : S, v : Value, opR : String, w : Value) : ISeq[(S, Value)] = {
    ilist(
      (s.addPathCondition(BinaryExp(opR, v, w)).requestInconsistencyCheck(), CI(1)),
      (s.addPathCondition(BinaryExp(comp(opR), v, w)).requestInconsistencyCheck(), CI(0)))
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