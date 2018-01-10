package com.bnmouli.misc.sauce
import scala.collection.immutable.Map
import scala.collection.mutable.MapBuilder

import com.bnmouli.misc.sauce.defs._

object TestMain {
  trait TopTrait {
    def pr = println("TopTrait")
  }

  object TopTrait {
    implicit val ib = reify(tie[TopTrait])
  }

  class Base extends TopTrait {
    override def pr = println("Base")
  }
  object Base {
    import Derived.ib
    implicit val ib = reify(tie[Base].to[Derived])
  }

  case class X(i: Int, j: Int)
  object X {
    implicit val ib = reify(tie[X].fun(apply _))
  }

  case class Derived(x: Long) extends Base {
    override def pr = println(s"Derived ${x}")
  }
  object Derived {
    implicit val ib: ImplicitBind[Derived] = reify(tie[Derived].fun(new Derived(_ : Long)))
  }

  class Leaf2(x: Int) extends TopTrait {
    override def pr = println(s"Leaf2 ${x}")
  }

  object Leaf2 {
    implicit val ib = reify(tie[Leaf2].fun(new Leaf2(_ : Int)))
  }

  class Other(injector: Injector) {
    val b = injector.get[Base]
    val d = injector.get[Derived]
    val li = injector.get[List[Int]]
    val ls = injector.get[List[String]]
  }
  object Other {
    implicit val ib = reify(tie[Other].fun(new Other(_ : Injector)))
  }

  class Temp2[A1, A2](val x: A2)
  object Temp2 {
    private val gt = TypeIdProvider.createTypeId()
    implicit def bi[A1, A2] = gtie[Temp2[A1, A2], GArg2[A1, A2]](gt)
  }

  class At[A1, A2](val x: A2)
  object At {
    private val gt = TypeIdProvider.createTypeId()
    implicit def bi[A1, A2] = gtie[At[A1, A2], GArg2[A1, A2]](gt).fun(new At[A1, A2](_ : A2))
  }

  case class Temp3[A1, A2](val x: A2, val y: A1)
  object Temp3 {
    private val gt = TypeIdProvider.createTypeId()
    implicit def bi[A1, A2] = gtie[Temp3[A1, A2], GArg2[A1, A2]](gt).fun((x: List[A2], y: A1) => {
        new Temp3[A1, A2](x(0), y)
      })
  }

  // Main
  def main(args: Array[String]): Unit = {
    val binder = new Binder
    // binder(bind[Base].to[Derived])
    binder(bind[TopTrait].to[Leaf2])
    binder(bind[Long].to[Int])
    binder(bind[List[Int]].expr(List(1, 2, 3)))
    binder(bind[List[Long]].expr(List(1000L, 2000L, 3000L)))
    binder(bind[List[String]].expr(List("Hello", "World")))
    
    def getLBase(d: Derived, d2: Derived, i: Int): List[Derived] = {
      println(s"Create called with ${i}")
      return List(d, d2)
    }
    binder(bind[List[Base]].fun(getLBase _))

    var counter = 1000
    def GetNext(): Int = {
      counter += 1
      return counter
    }

    binder(bind[Int].fun(GetNext _))

    val injector = binder.create
    val b = injector.get[Base]
    println("get Base")
    b.pr

    val atb  = injector.get[At[Long, Base]]
    atb.x.pr

    val t3  = injector.get[Temp3[Long, Base]]
    println(t3)

    val t = injector.get[TopTrait]
    t.pr

    val lb = injector.get[List[Base]]
    println("printing list")
    lb.foreach(_.pr)
    println("done.")

    val o = injector.get[Other]
    o.b.pr
    o.d.pr
    println(o.li)
    println(o.ls)

    println(s"typekey = ${getTypeId[At[Long, Base]]}")
    println(s"typekey = ${getTypeId[At[List[Int], Base]]}")

    println("X.i = " + injector.get[X].i)
    println(injector.get[Long])
  }
}
