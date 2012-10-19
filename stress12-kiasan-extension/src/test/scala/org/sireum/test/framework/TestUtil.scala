/*
Copyright (c) 2011-2012 Robby, Kansas State University.        
All rights reserved. This program and the accompanying materials      
are made available under the terms of the Eclipse Public License v1.0 
which accompanies this distribution, and is available at              
http://www.eclipse.org/legal/epl-v10.html                             
*/

package org.sireum.test.framework

import org.sireum.pilar.ast._
import org.sireum.pilar.parser._
import org.sireum.util._

object TestUtil {
  def parse[T <: PilarAstNode] //
  (source : Either[String, FileResourceUri], claz : Class[T]) =
    PilarParser.parseWithErrorAsString(source, claz)
}