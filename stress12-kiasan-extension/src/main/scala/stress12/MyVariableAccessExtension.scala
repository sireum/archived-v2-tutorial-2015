package stress12

import org.sireum.extension._
import org.sireum.extension.annotation._
import org.sireum.pilar.ast._
import org.sireum.pilar.eval._
import org.sireum.pilar.state._
import org.sireum.util._

object MyVariableAccessExtension extends ExtensionCompanion {
  def create[S <: State[S]](
    config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]]) =
    new MyVariableAccessExtension(config)

  val URI_PATH = "stress12/MyVariableAccessExtension"
}

class MyVariableAccessExtension[S <: State[S]](
  config : EvaluatorConfiguration[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]])
    extends Extension[S, Value, ISeq[(S, Value)], ISeq[(S, Boolean)], ISeq[S]] {

  def uriPath = MyVariableAccessExtension.URI_PATH

  def varUri(x : NameUser) = if (x.hasResourceInfo) x.resourceUri else x.name

  implicit def re2r(p : (S, Value)) = ilist(p)
  implicit def s2sr(s : S) = ilist(s)

  @VarLookup
  def variableLookup : (S, NameUser) --> ISeq[(S, Value)] = {
    case (s, x) => (s, s.variable(varUri(x)))
  }

  @VarUpdate
  def variableUpdate : (S, NameUser, Value) --> ISeq[S] = {
    case (s, x, v) => s.variable(varUri(x), v)
  }
}
