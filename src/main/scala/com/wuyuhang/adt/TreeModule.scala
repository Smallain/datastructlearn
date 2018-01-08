package com.wuyuhang.adt

object TreeModule {
	
	sealed trait MyTree[+T] {
		def value: T
		//面向对象的形式定义map处理抽象数据类型中的每一个值
		def map[R](f: T => R): MyTree[R]
	}
	
	//递归定义 left right
	case class Branch[T](left: MyTree[T], value: T, right: MyTree[T]) extends MyTree[T] {
		override def map[R](f: T => R): MyTree[R] = Branch(left.map(f), f(value), right.map(f))
	}
	
	case class Leaf[T](value: T) extends MyTree[T] {
		override def map[R](f: T => R): MyTree[R] = Leaf(f(value))
	}
	
	//	case object Empty extends MyTree[Nothing] {
	//		override def value: Nothing = Empty
	//	}
	
}
