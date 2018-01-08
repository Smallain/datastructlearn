package com.wuyuhang.functor

import com.wuyuhang.adt.ListModule._
import com.wuyuhang.adt.TreeModule._

//functor 的数学含义是可计算函子
//类型类
object FunctorModule {
	
	//有一个T类型的结构，T类型内部的类型我不去关心，这样就构造了一个类型的类，简称类型类
	trait MyFunctor[T[_]] {
		//定义一个map,用于处理T类型中的类型转换，就好像一个tree和一个list，我不用去关心tree和list结构的不同，我只要求操作可以深入
		//到具体类型的内部去处理里面的类型转换。为什么是类型转换呢？因为scala的数据类型不可变，只可能通过转换生成新的类型
		def map[A, B](t: T[A])(f: A => B): T[B]
	}
	
	implicit object FunctorMyTree extends MyFunctor[MyTree] {
		override def map[A, B](t: MyTree[A])(f: A => B): MyTree[B] = t match {
			case Leaf(v) => Leaf(f(v))
			case Branch(l, v, r) => Branch(map(l)(f), f(v), map(r)(f))
		}
	}
	
	implicit object FunctorMyList extends MyFunctor[MyList] {
		override def map[A, B](t: MyList[A])(f: A => B): MyList[B] = t match {
			case MyNil => MyNil
			case Cons(h, tail) => Cons(f(h), map(tail)(f))
		}
	}
	
}
