/* Функция split: (List[Int], Int => Boolean) => List[List[Int]],
 * выполняющая разбиение последовательности целых чисел на
 * подпоследовательности, разделённые числами, удовлетворяющими предикату.
 */

val split: (List[Int], Int => Boolean) => List[List[Int]] = {
    case (Nil,      p)               => Nil
    case (x::xs,    p) if(p(x))      => split(xs, p)
    case (x::xs,    p) if(xs == Nil) => List(List(x))
    case (x::y::xs, p) if(p(y))      => List(x)::split(xs, p)
    case (x::xs,    p)               => {
        val a::bs = split(xs, p)
        (x::a)::bs
    }
}

split(List(1, 2, 3, 4), _==1000)
split(List(1, 1000, 2, 3, 1000, 4), _==1000)
split(List(1000, 1, 1000, 2, 3, 1000, 4, 1000), _==1000)
