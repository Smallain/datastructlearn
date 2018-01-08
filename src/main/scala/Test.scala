
import com.wuyuhang.adt.TreeModule._
import com.wuyuhang.adt.ListModule._

object Test extends App {
	
	//List Struct
	println(MyNil)
	println(Cons(1, Cons(2, Cons(3, MyNil))))
	
	//Tree Struct
	println(Branch(Leaf(2), 1, Leaf(1)))
	
	
	//模式匹配取树结构中到值
	val tree: Branch[Int] = Branch(Leaf(2), 1, Leaf(1))
	
	tree match {
		case Branch(_, _, Leaf(v)) => println(s"leaf is $v")
	}
	
	//对于递归定义的抽象数据结构本能的需要定义一个递归获取的函数去应对抽象数据类型的结构
	def rightMost[T]: MyTree[T] => T = {
		//偏函数定义
		case Leaf(v) => v
		case Branch(_, _, right) => rightMost(right)
	}
	
	val bigTree: MyTree[Int] = Branch(Leaf(2), 1, Branch(Leaf(4), 5, Leaf(6)))
	
	println(s"right most value is ${rightMost(bigTree)}")
	
	//对于抽象数据类型中的数据进行类似map的操作可以进行处理的的方法
	
	//1.模式匹配和递归处理
	def allMulti2(tree: MyTree[Int]): MyTree[Int] = map1(tree)(_ * 2)
	
	//辅助函数类似map
	def map1[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree match {
		case Leaf(v) => Leaf(f(v))
		case Branch(l, v, r) => Branch(map1(l)(f), f(v), map1(r)(f))
	}
	
	println(s"1.模式匹配的处理方式:	${allMulti2((bigTree))}")
	
	
	//2.面向对象的操作方式，将map函数定义在抽象数据类型中
	
	def mapObject[A, B](tree: MyTree[A])(f: A => B): MyTree[B] = tree.map(f)
	
	def allMulti2Object(tree: MyTree[Int]): MyTree[Int] = mapObject(tree)(_ * 2)
	
	println(s"2.面向对象的处理方式:	${allMulti2Object((bigTree))}")
	
	//3.类型类MyFunctor的实现方式
	import com.wuyuhang.functor.FunctorModule._
	
	//类型类的好处是不像面向对象需要将tree 和 list 分别 extends functor，我想要引用是只需要在functor中添加然后在外边用到的地方定义就行了
	//在扩展代码是，这种方式不需要驱动tree和list的数据结构
	def mapFunctor[T[_], A, B](t: T[A])(f: A => B)(implicit functor: MyFunctor[T]): T[B] = {
		functor.map(t)(f)
	}
	
	println(s"3.类型类的实现方式:	${mapFunctor(bigTree)(_ * 4)}")
	
	val listcons: MyList[Int] = Cons(1, Cons(2, Cons(3, MyNil)))
	
	println(s"3.类型类的实现方式(MyList):	${mapFunctor(listcons)(_ * 4)}")
}
