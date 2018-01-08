package com.wuyuhang.adt

object ListModule {
	
	sealed trait MyList[+T]{
		def head:T
		def tail:MyList[T]
	}
	
	case object MyNil extends MyList[Nothing] {
		override def head: Nothing = throw new IllegalArgumentException("no head no empty List")
		
		override def tail: MyList[Nothing] = throw new IllegalArgumentException("no tail no empty List")
	}
	//递归定义tail
	case class Cons[T](head: T, tail: MyList[T]) extends MyList[T]
	
}
