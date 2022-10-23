package osiris.utilities.serialization.v2

import osiris.function.{Constant, DeadInput, Identity, VectorFunction}
import osiris.function.bilinear.BilinearFunction
import osiris.function.linear.LinearFunction
import osiris.morphism.Isomorphism
import osiris._
import osiris.utilities.serialization.v2

object Function {

  object constants {

    val identity:Byte = 1
    val constant:Byte = 2
    val deadInput:Byte = 3

    val composition:Byte = 10
    val composedLinear:Byte = 11
    val linearComposeBilinear:Byte = 12
    val bilinearLeftComposeLinear:Byte = 13
    val bilinearRightComposeLinear:Byte = 14

    val map:Byte = 20
    val rowMap:Byte = 21
    val colMap:Byte = 22
    val biMap:Byte = 23
    val lMap:Byte = 24
    val rMap:Byte = 25

    val elemWise:Byte = 26
    val rowWise:Byte = 27
    val colWise:Byte = 28

    val rowMappedLinear:Byte = 30
    val colMappedLinear:Byte = 31
    val biMappedLinear:Byte = 32
    val lMappedLinear:Byte = 33
    val rMappedLinear:Byte = 34

    val rowWiseBilinear:Byte = 35
    val colWiseBilinear:Byte = 36

    val simpleLinear:Byte = 40
    val scalarProduct:Byte = 41

    val addition:Byte = 45
    val sum:Byte = 46
    val rowSum:Byte = 47
    val colSum:Byte = 48

    val copy:Byte = 51
    val fill:Byte = 52
    val rowCopy:Byte = 53
    val colCopy:Byte = 54

    val permute:Byte = 61
    val extract:Byte = 62
    val pad:Byte = 63

    val dft:Byte = 67

    val layeredBilinear:Byte = 70
    val multiplication:Byte = 71
    val complexMultiplication:Byte = 72
    val leftScalarProduct:Byte = 73
    val rightScalarProduct:Byte = 74
    val elementWiseMultiplication:Byte = 75
    val innerProduct:Byte = 76
    val outerProduct:Byte = 77
    val vectorMatrixProduct:Byte = 78
    val matrixMatrixProduct:Byte = 79
    val matrixVectorProduct:Byte = 80
    val convolution:Byte = 8

  }

  def deserialize[I,J,S](bytes:Iterator[Byte]):VectorFunction[I,J,S] = {
    val scalarSpace = v2.ScalarSpace.deserialize(Iterator(bytes.next()))
    deserialize[I,J,S](scalarSpace.asInstanceOf[ScalarSpace[S]],bytes)
  }

  def deserialize[I,J,S](S:ScalarSpace[S], bytes:Iterator[Byte]):VectorFunction[I,J,S] = {
    (bytes.next() match {
      case v2.Function.constants.identity => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new Identity(shape --> S)
      }
      case v2.Function.constants.constant => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = v2.Vector.deserialize(shape-->S,bytes)
        new Constant(c)
      }
      case v2.Function.constants.deadInput => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new DeadInput(shape --> S)
      }
      case v2.Function.constants.composition => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[VectorFunction[_,A,S]]
        val gg = g.asInstanceOf[VectorFunction[A,_,S]]
        ff << gg
      }
      case v2.Function.constants.composedLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[LinearFunction[_,A,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff << gg
      }
      case v2.Function.constants.linearComposeBilinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[LinearFunction[_,A,S]]
        val gg = g.asInstanceOf[BilinearFunction[A,_,_,S]]
        ff << gg
      }
      case v2.Function.constants.bilinearLeftComposeLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain.asPairSpace.left,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[BilinearFunction[_,A,_,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff <</ gg
      }
      case v2.Function.constants.bilinearRightComposeLinear => {
        val f = deserialize(S,bytes)
        val g = deserialize(S,bytes)
        val middle = utilities.same(f.domain.asPairSpace.right,g.target)
        type A = middle.shape.Type
        val ff = f.asInstanceOf[BilinearFunction[_,_,A,S]]
        val gg = g.asInstanceOf[LinearFunction[A,_,S]]
        ff <<\ gg
      }
      case v2.Function.constants.map => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        val f = utilities.serialization.v2.ScalarFunction.deserialize(S,bytes)
        new osiris.function.map.Map(shape --> S,f)
      }
      case v2.Function.constants.rowMap => {
        val outerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        new osiris.function.map.RowMap[O,f.target.shape.Type,f.domain.shape.Type,S](o-->S,f)
      }
      case v2.Function.constants.colMap => {
        val innerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type I = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[I]]
        val f = deserialize(S,bytes)
        new osiris.function.map.ColMap[f.target.shape.Type,f.domain.shape.Type,I,S](i-->S,f)
      }
      case v2.Function.constants.biMap => {
        val l = deserialize(S,bytes)
        val r = deserialize(S,bytes)
        new osiris.function.map.BiMap[l.target.shape.Type,l.domain.shape.Type,r.target.shape.Type,r.domain.shape.Type,S](l,r)
      }
      case v2.Function.constants.lMap => {
        val l = deserialize(S,bytes)
        val r = utilities.serialization.v2.Shape.deserialize(bytes)
        type R = r.Type
        val rr = r.asInstanceOf[shape.Shape[R]]
        new osiris.function.map.LMap[l.target.shape.Type,l.domain.shape.Type,R,S](l,rr --> S)
      }
      case v2.Function.constants.rMap => {
        val l = utilities.serialization.v2.Shape.deserialize(bytes)
        type L = l.Type
        val ll = l.asInstanceOf[shape.Shape[L]]
        val r = deserialize(S,bytes)
        new osiris.function.map.RMap[L,r.target.shape.Type,r.domain.shape.Type,S](ll --> S,r)
      }
      case v2.Function.constants.elemWise => {
        val targetShape = utilities.serialization.v2.Shape.deserialize(bytes)
        val f = deserialize(S,bytes)
        new osiris.function.map.ElemWise(targetShape-->S,f.asInstanceOf[VectorFunction[Unit,+[Unit,Unit],S]])
      }
      case v2.Function.constants.rowWise => {
        val outerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.RowWise[O,I,L,R,S](o-->S,f.asInstanceOf[VectorFunction[I,+[L,R],S]])
      }
      case v2.Function.constants.colWise => {
        val innerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type J = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[J]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.map.ColWise[I,L,R,J,S](i-->S,f.asInstanceOf[VectorFunction[I,+[L,R],S]])
      }
      case v2.Function.constants.rowMappedLinear => {
        val outerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        val f = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.RowMappedLinear(outerShape-->S,f)
      }
      case v2.Function.constants.colMappedLinear => {
        val innerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        val f = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.ColMappedLinear(innerShape-->S,f)
      }
      case v2.Function.constants.biMappedLinear => {
        val l = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        val r = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.BiMappedLinear(l,r)
      }
      case v2.Function.constants.lMappedLinear => {
        val l = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        val r = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.map.LMappedLinear(l,r-->S)
      }
      case v2.Function.constants.rMappedLinear => {
        val l = utilities.serialization.v2.Shape.deserialize(bytes)
        val r = deserialize(S,bytes).asInstanceOf[LinearFunction[_,_,S]]
        new osiris.function.linear.map.RMappedLinear(l-->S,r)
      }
      case v2.Function.constants.rowWiseBilinear => {
        val outerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type O = outerShape.Type
        val o = outerShape.asInstanceOf[shape.Shape[O]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.bilinear.RowWiseBilinear[O,I,L,R,S](o-->S,f.asInstanceOf[BilinearFunction[I,L,R,S]])
      }
      case v2.Function.constants.colWiseBilinear => {
        val innerShape = utilities.serialization.v2.Shape.deserialize(bytes)
        type J = innerShape.Type
        val i = innerShape.asInstanceOf[shape.Shape[J]]
        val f = deserialize(S,bytes)
        val domain = f.domain.asPairSpace
        type I = f.target.shape.Type
        type L = domain.left.shape.Type
        type R = domain.right.shape.Type
        new osiris.function.bilinear.ColWiseBilinear[I,J,L,R,S](i-->S,f.asInstanceOf[BilinearFunction[I,L,R,S]])
      }
      case v2.Function.constants.simpleLinear => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Product[_,_]]
        type A = shape.a.Type
        type B = shape.b.Type
        val space = (shape.asInstanceOf[osiris.shape.Product[A,B]] --> S)
        val k = v2.Vector.deserialize(space,bytes)
        new osiris.function.linear.SimpleLinear[A,B,S](k.asMatrix)
      }
      case v2.Function.constants.scalarProduct => {
        val shape = v2.Shape.deserialize(bytes)
        val k = Scalar.deserialize(S,bytes)
        new osiris.function.linear.ScalarProduct(shape-->S,k)
      }
      case v2.Function.constants.addition => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.Addition(shape --> S)
      }
      case v2.Function.constants.sum => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.Sum(shape --> S)
      }
      case v2.Function.constants.rowSum => {
        val outer = utilities.serialization.v2.Shape.deserialize(bytes)
        val inner = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.RowSum(outer-->S,inner-->S)
      }
      case v2.Function.constants.colSum => {
        val outer = utilities.serialization.v2.Shape.deserialize(bytes)
        val inner = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.ColSum(outer-->S,inner-->S)
      }
      case v2.Function.constants.copy => {
        val domain = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.Copy(domain-->S)
      }
      case v2.Function.constants.fill => {
        val target = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.Fill(target-->S)
      }
      case v2.Function.constants.rowCopy => {
        val outer = utilities.serialization.v2.Shape.deserialize(bytes)
        val inner = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.RowCopy(outer-->S,inner-->S)
      }
      case v2.Function.constants.colCopy => {
        val outer = utilities.serialization.v2.Shape.deserialize(bytes)
        val inner = utilities.serialization.v2.Shape.deserialize(bytes)
        new osiris.function.linear.reindex.ColCopy(outer-->S,inner-->S)
      }
      case v2.Function.constants.permute => {
        val f = utilities.serialization.v2.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Permute[I,J,S](
          S,
          f.asInstanceOf[Isomorphism[I,J]]
        )
      }
      case v2.Function.constants.extract => {
        val f = utilities.serialization.v2.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Extract[I,J,S](
          S,
          f.asInstanceOf[osiris.morphism.Monomorphism[I,J]]
        )
      }
      case v2.Function.constants.pad => {
        val f = utilities.serialization.v2.Morphism.deserialize(bytes)
        type I = f.domain.Type
        type J = f.target.Type
        new osiris.function.linear.reindex.Pad[J,I,S](
          S,
          f.asInstanceOf[osiris.morphism.Monomorphism[I,J]]
        )
      }
      case v2.Function.constants.dft => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        new osiris.function.linear.DiscreteFourierTransform[S](shape-->S)
      }
      case v2.Function.constants.layeredBilinear => {
        val f = deserialize(S,bytes).asInstanceOf[BilinearFunction[_,_,_,S]]
        val g = deserialize(S,bytes).asInstanceOf[BilinearFunction[_,_,_,S]]
        f & g
      }
      case v2.Function.constants.multiplication => {
        new function.bilinear.Multiplication(S)
      }
      case v2.Function.constants.complexMultiplication => {
        new function.bilinear.ComplexMultiplication(S)
      }
      case v2.Function.constants.leftScalarProduct => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.LeftScalarProduct(shape --> S)
      }
      case v2.Function.constants.rightScalarProduct => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.RightScalarProduct(shape --> S)
      }
      case v2.Function.constants.elementWiseMultiplication => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.ElementWiseMultiplication(shape --> S)
      }
      case v2.Function.constants.innerProduct => {
        val shape = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.InnerProduct(shape --> S)
      }
      case v2.Function.constants.outerProduct => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.OuterProduct(a-->S,b-->S)
      }
      case v2.Function.constants.vectorMatrixProduct => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.VectorMatrixProduct(a-->S,b-->S)
      }
      case v2.Function.constants.matrixMatrixProduct => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        val c = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.MatrixMatrixProduct(a-->S,b-->S,c-->S)
      }
      case v2.Function.constants.matrixVectorProduct => {
        val a = utilities.serialization.v2.Shape.deserialize(bytes)
        val b = utilities.serialization.v2.Shape.deserialize(bytes)
        new function.bilinear.MatrixVectorProduct(a-->S,b-->S)
      }
      case v2.Function.constants.convolution => {
        val left = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        val right = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        val target = utilities.serialization.v2.Shape.deserialize(bytes).asInstanceOf[osiris.shape.Range]
        implicit val tag = S.tag
        new function.bilinear.Convolution[S](left-->S,right-->S,target-->S)
      }
    }).asInstanceOf[VectorFunction[I,J,S]]
  }

}