/*
 *     Copyright (C) 2015  morinb
 *     https://github.com/morinb
 *
 *     This library is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU Lesser General Public
 *     License as published by the Free Software Foundation; either
 *     version 2.1 of the License, or (at your option) any later version.
 *
 *     This library is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *     Lesser General Public License for more details.
 *
 *     You should have received a copy of the GNU Lesser General Public
 *     License along with this library; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
 */

package org.bm

import org.bm.scalacompute.evaluator.Evaluator

import scala.collection.mutable
import scala.language.implicitConversions

/**
 *
 * @author morinb.
 */
package object scalacompute {
  def isVariable(item: String, map: Map[String, String]): Boolean = map.contains(item)

  def isConstant(item: String): Boolean = Constants.constantsWithName.exists(tuple => tuple._1.contains(item.toLowerCase))

  def isOperator(item: String): Boolean = Operators.operatorsWithName.exists(tuple => tuple._1.contains(item.toLowerCase))

  def isFunction(item: String): Boolean = Functions.functionsWithName.exists(tuple => tuple._1.contains(item.toLowerCase))

  def isNumber(item: String): Boolean = try {
    item.toDouble
    true
  } catch {
    case _: Throwable => false
  }

  object Implicits {
    implicit def constantToDouble(c: Constant): Double = c.doubleValue

    implicit def enrichDouble(d: Double): RichDouble = new RichDouble(d)

    class RichDouble(d: Double) {
      def **(other: Double): Double = {
        Math.pow(d, other)
      }
    }

    implicit def enrichStack[T](s: mutable.Stack[T]): RichMutableStack[T] = new RichMutableStack[T](s)

    class RichMutableStack[T](s: mutable.Stack[T]) {
      def popUntilEqualsTo(item: T): Seq[T] = {
        var accu: List[T] = Nil
        do {
          accu = s.pop :: accu
        } while (s.top != item)

        accu
      }

      def popx(nb: Int): Seq[T] = for (i <- 0 until nb) yield s.pop()
    }

  }

  trait Token

  trait Associative {
    def leftAssociative: Boolean

    def rightAssociative = !leftAssociative
  }

  trait LeftAssociative extends Associative {
    def leftAssociative: Boolean = true
  }

  trait RightAssociative extends Associative {
    def leftAssociative: Boolean = false
  }

  trait Argumented {
    def argsNumber: Int
  }

  trait MonoArgument extends Argumented {
    def argsNumber = 1
  }

  trait DualArguments extends Argumented {
    def argsNumber = 2
  }

  trait ManyArguments extends Argumented {
    def argsNumber = -1
  }

  trait Precedence {
    def precedence: Int
  }

  trait PrecedenceForUnary extends Precedence {
    def precedence: Int = 14
  }

  trait PrecedenceMulDivMod extends Precedence {
    def precedence: Int = 13
  }

  trait PrecedenceAdditonSubtract extends Precedence {
    def precedence: Int = 12
  }


  trait Executable {
    def calc(operatorName: String, operands: Seq[String], requiredArgsNumber: Int)(exec: (Seq[String]) => String): String = {
      require(operands.length == requiredArgsNumber, s"Operator $operatorName requires $requiredArgsNumber operand${if (requiredArgsNumber > 1) "s"}")
      exec(operands)
    }

    def executeMethod: (Seq[String]) => String
  }

  trait Value {
    val value: String
  }

  trait DoubleValue {
    val doubleValue: Double
  }

  trait LowerCaseName {
    def lowerCaseName: String
  }

  trait Function extends Token with Argumented with Executable with LowerCaseName

  trait Operator extends Token with Associative with Argumented with Precedence with Executable with LowerCaseName

  trait Constant extends Token with LowerCaseName with Value with DoubleValue {
    self =>
    override lazy val value: String = self.doubleValue.toString
  }

  object Functions {

    def functionsWithName: Seq[(String, Function)] = getFunctions.map(func => (func.lowerCaseName, func))

    def apply(name: String): Function = {
      val v = functionsWithName.filter(tuple => tuple._1.contains(name.toLowerCase))
      require(v.size < 2, s"Found more than one function for name '$name'")
      require(v.nonEmpty, s"Found no function for name '$name'")
      v.head._2
    }

    private[this] var functions = List[Function]()

    def addFunctions(fs: Function*): Unit = {
      fs foreach (f => functions = f :: functions)
    }

    addFunctions(SQRT, LOG, EXP)

    def getFunctions: Seq[Function] = functions

    object SQRT extends Function with MonoArgument with LowerCaseName {
      self =>
      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(Seq => Math.sqrt(Seq.head.toDouble).toString)

      override def lowerCaseName: String = "sqrt"
    }

    object LOG extends Function with MonoArgument with LowerCaseName {
      self =>
      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(Seq => Math.log10(Seq.head.toDouble).toString)

      override def lowerCaseName: String = "log"
    }

    object EXP extends Function with MonoArgument with LowerCaseName {
      self =>
      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(Seq => Math.exp(Seq.head.toDouble).toString)

      override def lowerCaseName: String = "exp"
    }

  }

  object Operators {

    def operatorsWithName: Seq[(String, Operator)] = getOperators.map(op => (op.lowerCaseName, op))

    private[this] var operators = List[Operator]()

    def addOperators(os: Operator*): Unit = {
      os foreach (o => operators = o :: operators)
    }

    def getOperators: Seq[Operator] = operators

    // order is important, MINUS() must be before NEGATE()
    addOperators(PLUS, MINUS, TIMES, DIVIDE, NEGATE, MODULO, POWER)

    def apply(name: String): Operator = {
      val v = operatorsWithName.filter(tuple => tuple._1.contains(name.toLowerCase))
      require(v.size < 2, s"Found more than one operator for name '$name'")
      require(v.nonEmpty, s"Found no operator for name '$name'")
      v.head._2
    }

    object PLUS extends Operator with LeftAssociative with DualArguments with PrecedenceAdditonSubtract with LowerCaseName {
      self =>
      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(_.map(_.toDouble).sum.toString)

      override def lowerCaseName: String = "+"
    }

    object MINUS extends Operator with LeftAssociative with DualArguments with PrecedenceAdditonSubtract with LowerCaseName {
      self =>

      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(_.map(_.toDouble).foldRight(0.0)(_ - _).toString)

      override def lowerCaseName: String = "-"
    }

    object TIMES extends Operator with DualArguments with LeftAssociative with PrecedenceMulDivMod with LowerCaseName {
      self =>

      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(_.map(_.toDouble).foldLeft(1.0)(_ * _).toString)

      override def lowerCaseName: String = "*"
    }

    object DIVIDE extends Operator with LeftAssociative with DualArguments with PrecedenceMulDivMod with LowerCaseName {
      self =>

      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(_.map(_.toDouble).foldRight(1.0)(_ / _).toString)

      override def lowerCaseName: String = "/"
    }

    object NEGATE extends Operator with LeftAssociative with MonoArgument with PrecedenceForUnary with LowerCaseName {
      self =>

      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(Seq => (-Seq.head.toDouble).toString)

      override def lowerCaseName: String = "~"
    }

    object MODULO extends Operator with LeftAssociative with DualArguments with PrecedenceMulDivMod with LowerCaseName {
      self =>
      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber) { Seq =>
          val op = Seq.map(_.toDouble)
          (op(0) % op(1)).toString
        }

      override def lowerCaseName: String = "%"
    }

    object POWER extends Operator with RightAssociative with DualArguments with PrecedenceForUnary with LowerCaseName {
      self =>

      import Implicits.enrichDouble

      override def executeMethod: (Seq[String]) => String =
        calc(self.getClass.getName, _, argsNumber)(_.map(_.toDouble).foldRight(1.0)(_ ** _).toString) // use implicit to convert to RichDouble which defines ** method

      override def lowerCaseName: String = "^"
    }


  }

  object Constants {
    private[this] var constants = List[Constant]()

    def constantsWithName: Seq[(String, Constant)] = getConstants.map(op => (op.lowerCaseName, op))

    def addConstants(cs: Constant*): Unit = {
      cs foreach (c => constants = c :: constants)
    }

    def getConstants: Seq[Constant] = constants

    def apply(name: String): Constant = {
      val v = constantsWithName.filter(tuple => tuple._1.contains(name.toLowerCase))
      require(v.size < 2, s"Found more than one constant for name '$name'")
      require(v.nonEmpty, s"Found no constant for name '$name'")
      v.head._2
    }

    addConstants(PI, PHI, GAMMA)

    object PI extends Constant {
      override def lowerCaseName: String = "pi"

      override val doubleValue: Double = Math.PI
    }

    object E extends Constant {
      override def lowerCaseName: String = "e"

      override val doubleValue: Double = Math.E
    }

    object PHI extends Constant {
      override val doubleValue: Double = (1 + Math.sqrt(5)) / 2

      override def lowerCaseName: String = "phi"
    }

    object GAMMA extends Constant {
      override val doubleValue: Double = 0.5772156649015329

      override def lowerCaseName: String = "gamma"

    }

  }

}
