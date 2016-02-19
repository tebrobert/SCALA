/* Двоичное неотрицательное число произвольной разрядности с операциями
 * сложения и умножения.
 */

import scala.math

class Num(px: String){
    /*private*/ val x = BigInt(px, 2)
    def value = x.toString(2)
    def +(x: Num) = new Num(this.x + x.x)
    def *(x: Num) = new Num(this.x * x.x)
    def this(x: BigInt) = this(x.toString(2))
}
