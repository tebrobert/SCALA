/* Двоичное неотрицательное число произвольной разрядности с операциями
 * сложения и умножения.
 */

class Num(px: String){
    /*private*/ val iS = 32
    /*private*/ val iL = 1.toLong << iS
    /*private*/ val pm = (x: Int) => x.toLong.&(1.toLong.<<(32)-1)
    /*private*/ val toList: String => List[Int] = {
        case "" => Nil
        case s if(s.length >= iS) => val sL = s.length
                                     val a = Integer.parseInt(s.substring(sL - (iS - 1)), 2)
                                     val b = Integer.parseInt(s.substring(sL - iS, sL - (iS - 1)), 2)
                                     (a+(b<<(iS-1)))::toList(s.substring(0, sL - iS))
        case s => List(Integer.parseInt(s, 2))
    }
    /*private*/ val L = toList(px)
    /*private*/ val toValue: List[Int] => String = {
        case x::xs  =>  val s = Integer.toBinaryString(x)
                        toValue(xs) + ("0" * (iS-s.length)) + s
        case Nil    =>  ""
    }
    /*private*/ val sumList: (List[Int], List[Int], Int)=>List[Int] = {
        case (x::xs, y::ys, c) => val a: Long = (pm(x)+pm(y)+pm(c))
                                  (a%iL).toInt::sumList(xs,ys, (a/iL).toInt)
        case (Nil, y::ys, c)   => val a: Long = (pm(y)+pm(c))
                                  (a%iL).toInt::sumList(ys, Nil, (a/iL).toInt)
        case (x::xs, Nil, c)   => val a: Long = (pm(x)+pm(c))
                                  (a%iL).toInt::sumList(xs, Nil, (a/iL).toInt)
        case (Nil, Nil, 0)     => Nil
        case (Nil, Nil, c)     => List(c)
    }
    /*private*/ val mulListInt: (List[Int], Int, Int)=>List[Int] = {
        case (x::xs, y, c)  => val a: Long = (pm(x)*pm(y)+pm(c))
                               (a%iL).toInt :: mulListInt(xs, y, (a/iL).toInt)
        case (Nil, y, 0)    => Nil
        case (Nil, y, c)    => List(c)
    }
    /*private*/ val mulList: (List[Int], List[Int])=>List[Int] = {
        case (Nil, ys)      =>  Nil
        case (x, Nil)       =>  Nil
        case (x::xs, ys)    =>  val a = mulListInt(ys, x, 0)
                                sumList(0::mulList(xs, ys), a, 0)
    }
    
    def value = toValue(L)
    def +(x: Num) = new Num(toValue(sumList(this.L, x.L, 0)))
    def *(x: Int) = new Num(toValue(mulListInt(this.L, x, 0)))
    def *(x: Num) = new Num(toValue(mulList(this.L, x.L)))
    def this(x: Int) = this(x.toBinaryString)
}

// ex1
val a = new Num("10000")
val b = new Num("11000")
val c = a + b
val d = a * b
a.value
b.value
c.value
d.value
a.L
b.L
c.L
d.L

// ex2
val a = new Num("100000000")
val b = new Num("110000000")
val c = a + b
val d = a * b
a.value
b.value
c.value
d.value
a.L
b.L
c.L
d.L

// ex3
val a = new Num("10000000000000000000000000000000000000000")
val b = new Num("11000000000000000000000000000000000000000")
val c = a + b
val d = a * b
a.value
b.value
c.value
d.value
a.L
b.L
c.L
d.L
