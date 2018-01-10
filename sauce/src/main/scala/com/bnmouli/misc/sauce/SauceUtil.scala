package com.bnmouli.misc.sauce

import scala.collection.immutable.Map
import scala.collection.mutable.MapBuilder

package defs {}
package object defs {
  class Injector private (builder: TypeMapBuilder, keys: List[Int]) {
    builder += (keys -> new BindInjector(keys, this))
    val bindings = builder.result()
    // Can throw UnboundException
    def get[T](implicit ib: ImplicitBind[T]): T = {
      return bindings.getOrElse(ib.keys, ib).asInstanceOf[BindTrait[T]].get(this)
    }
  }

  case class UnboundException(
    private val message: String = "", 
    private val cause: Throwable = None.orNull)
  extends Exception(message, cause) 

  class Binder {
    val builder = new TypeMapBuilder(Map.empty)

    def apply[F, C](bi: BindInfo[F, NilKeyTag, C])(implicit invoker: Invoker[F, C]): Unit = {
      builder += (bi.keys -> toDefaultBind(bi, nilkey, invoker))
    }

    def create() = Injector(builder)
  }

  def bind[F](implicit df: ImplicitBind[F]): BindFrom[F, NilKeyTag] = {
    return new BindFrom[F, NilKeyTag](df.keys)
  }

  class BindFrom[F, GA](k: List[Int]) extends BindInfo[F, GA, () => F](k, ThrowUnbound[F] _) {
    def to[T <% F] = {
      new BindInfo[F, GA, (T) => F](keys, (x: T) => x: F)
    }

    def expr[T <% F](expr: => T) = {
      new BindInfo[F, GA, () => F](keys, () => expr: F)
    }

    def fun[T <% F](fun: () => T) = {
      new BindInfo[F, GA, () => F](keys, () => fun(): F)
    }

    def fun[T <% F, A1](fun: (A1) => T) = {
      new BindInfo[F, GA, (A1) => F](keys, (a1: A1) => fun(a1): F)
    }

    def fun[T <% F, A1, A2](fun: (A1, A2) => T) = {
      new BindInfo[F, GA, (A1, A2) => F](keys, (a1: A1, a2: A2) => fun(a1, a2): F)
    }

    def fun[T <% F, A1, A2, A3](fun: (A1, A2, A3) => T) = {
      new BindInfo[F, GA, (A1, A2, A3) => F](keys, (a1: A1, a2: A2, a3: A3) => {
          fun(a1, a2, a3): F
      })
    }
  }

  // Generic arguments.
  private [defs] trait GArgs
  class GArg1[G1] extends GArgs
  class GArg2[G1, G2] extends GArgs
  class GArg3[G1, G2, G3] extends GArgs

  // Type key
  def gtie[T, GA](tid: Int) = new BindFrom[T, GA](List(tid))

  object TypeIdProvider {
    private var tid: Int = 0
    def createTypeId() : Int = { tid += 1; tid }
  }
  def tie[T] = gtie[T, NilKeyTag](TypeIdProvider.createTypeId())

  def reify[T, C](bi: BindInfo[T, NilKeyTag, C])(implicit iv: Invoker[T, C]): ImplicitBind[T] = {
    toDefaultBind(bi, nilkey, iv)
  }

  // Binding common collections
  private val listId = TypeIdProvider.createTypeId()
  implicit def listKey[G1] = gtie[List[G1], GArg1[G1]](listId)


  // PRIVATE TYPES AND METHODS
  private [defs] class TypeTag[T]
  class BindInfo[T, GA, C] private[defs] (val keys: List[Int], val fun: C)

  private [defs] class NilKeyTag
  private [defs] class InvalidFun

  private def ThrowUnbound[T](): T = throw new UnboundException

  private [defs] class GenericKey[T](val keys: List[Int])

  private [defs] trait BindTrait[+T] {
    val keys = List(0)
    def get(injector: Injector): T
  }

  private class BindInjector(k: List[Int], val injector: Injector) extends BindTrait[Injector] {
    override val keys = k
    override def get(injector: Injector) = injector
  }

  private type TypeMap = Map[List[Int], BindTrait[Any]]
  private type TypeMapBuilder =
      MapBuilder[List[Int], BindTrait[Any], Map[List[Int], BindTrait[Any]]]

  // Note we explicitly avoid +T to ensure implicit arguments pick up exact match.
  abstract class ImplicitBind[T] extends BindTrait[T]

  implicit def nilkey = new GenericKey[NilKeyTag](Nil)

  implicit def toGenericKey1[G1](implicit
      a1: ImplicitBind[G1]) = new GenericKey[GArg1[G1]](a1.keys)

  implicit def toGenericKey2[G1, G2](implicit
      a1: ImplicitBind[G1],
      a2: ImplicitBind[G2]) = new GenericKey[GArg2[G1, G2]](a1.keys ::: a2.keys)

  implicit def toGenericKey3[G1, G2, G3](implicit
      a1: ImplicitBind[G1],
      a2: ImplicitBind[G2],
      a3: ImplicitBind[G3]) = new GenericKey[GArg3[G1, G2, G3]](a1.keys ::: a2.keys ::: a3.keys)

  private [defs] trait Invoker[T, C] {
    def invoke(fun: C, injector: Injector): T
  }

  implicit def toInvoker0[T] = new Invoker[T, () => T] {
    override def invoke(fun: () => T, injector: Injector) = fun()
  }

  implicit def toInvoker1[T, A1](implicit
      a1: ImplicitBind[A1]) = new Invoker[T, (A1) => T] {
    override def invoke(fun: (A1) => T, injector: Injector): T= fun(
      injector.get[A1](a1))
  }

  implicit def toInvoker2[T, A1, A2](implicit
      a1: ImplicitBind[A1],
      a2: ImplicitBind[A2]) = new Invoker[T, (A1, A2) => T] {
    override def invoke(fun: (A1, A2) => T, injector: Injector): T= fun(
      injector.get[A1](a1),
      injector.get[A2](a2))
  }

  implicit def toInvoker3[T, A1, A2, A3](implicit
      a1: ImplicitBind[A1],
      a2: ImplicitBind[A2],
      a3: ImplicitBind[A3]) = new Invoker[T, (A1, A2, A3) => T] {
    override def invoke(fun: (A1, A2, A3) => T, injector: Injector): T= fun(
      injector.get[A1](a1),
      injector.get[A2](a2),
      injector.get[A3](a3))
  }

  implicit def toDefaultBind[T, GA, C](implicit
      bi: BindInfo[T, GA, C],
      gk: GenericKey[GA],
      iv: Invoker[T, C]): ImplicitBind[T] = {
    return new ImplicitBind[T] {
      override val keys = bi.keys ::: gk.keys
      override def get(injector: Injector) = iv.invoke(bi.fun, injector)
    }
  }

  private object Injector {
    implicit val ib = tie[Injector]
    def apply(builder: TypeMapBuilder): Injector = {
      val di = implicitly[ImplicitBind[Injector]]
      return new Injector(builder, di.keys)
    }
  }

  // Debug only
  def getTypeId[T](implicit ib: ImplicitBind[T]): List[Int] = {
    return ib.keys
  }

  // Binding premitive types
  implicit val saInt = reify(tie[Int])
  implicit val saLong = reify(tie[Long])
  implicit val saString = reify(tie[String])
}
