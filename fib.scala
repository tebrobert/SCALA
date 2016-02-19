/* Функция fib: (Int, Int => Boolean, Int, Int) => List[Int], порождающая
 * последовательность чисел Фибоначчи, не превышающих заданного целого числа и
 * удовлетворяющих некоторому предикату.
 */

val f: (Int, Int => Boolean, Int, Int) => List[Int] = {
    case (n, p, a, b) if (a + b > n) => Nil
    case (n, p, a, b) if (p(a + b))  => (a + b)::f(n, p, b, a + b)
    case (n, p, a, b)                => f(n, p, b, a + b)
}

val fib: (Int, Int => Boolean) => List[Int] = {
    (n, p) => f(n, p, 1, 0)
}
