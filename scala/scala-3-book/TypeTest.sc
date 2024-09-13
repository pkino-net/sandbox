import scala.reflect.*

class A
class B extends A

def wrongMatch[T <: A](value: A): Unit =
  value match {
    case _: T => println("match")
    case _ => println("unmatch")
  }

def matchWithClassTag[T <: A : ClassTag](value: A) = 
  value match {
    case _: T => println("match")
    case _ => println("unmatch")
  }

given TypeTest[A, B] with {
  def unapply(x: A): Option[x.type & B] = x match {
    case _: B => Some(x.asInstanceOf[x.type & B])
    case _ => None
  }
}

def matchWithTypeTest[T <: A](value: A)(using TypeTest[A, B]) =
  value match {
    case _: T => println("match")
    case _ => println("umnatch")
  }


val a = A()

wrongMatch[B](a)
matchWithClassTag[B](a)
matchWithTypeTest[B](a)

