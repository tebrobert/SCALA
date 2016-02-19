import scala.math

class Node(val Left: List[Node], val Right: List[Node], val value: Double){
    def this(v: Double) = this(Nil, Nil, v)
}

// Interval: [Left; Right)
class Interval(val Left: Int, val Right: Int){
    val Length = Right - Left
    val Mid = Left + Length / 2
    def split(P: Int): List[Interval] = List(new Interval(Left, P), new Interval(P, Right))
    def has(P: Int): Boolean = (P >= Left) && (P < Right)
    def has(I: Interval): Boolean = has(I.Left) && has(I.Right - 1)
    def ==(I: Interval): Boolean = (Left == I.Left) && (Right == I.Right)
}

class SegmentTree private (
    private val Root: List[Node],
    val Length: Int,
    val Capacity: Int,
    val Height: Int
){
    def this() = this(List(new Node(Double.NegativeInfinity)), 0, 1, 0)
    
    // F - associative, e - right-neutral
    // val F: (Double, Double) => Double = (a, b) => math.min(a, b); val e = Double.PositiveInfinity
    // val F: (Double, Double) => Double = (a, b) => math.max(a, b); val e = Double.NegativeInfinity
    val F: (Double  , Double) => Double = (a, b) => a + b; val e = 0
    // val F: (Double, Double) => Double = (a, b) => a * b; val e = 1
    // val F: (Double, Double) => Double = (a, b) => a; val e = 666
    
    private val crEST: Int => List[Node] = {
        case 0 => List(new Node(e))
        case x => List(new Node(crEST(x - 1), crEST(x - 1), e))
    }
    private val wR: (Int, Int) => Boolean = {
        (i, h) => ( (i & ((1 << (h - 1))) ) == (1 << (h - 1)) )
    }
    private val getNode: (Int, List[Node], Int, Int) => List[Node] = {
        case (_, n, _, 0)              => n
        case (_, n, 0, _)              => n
        case (i, n, h, m) if(wR(i, h)) => getNode(i, n(0).Right, h - 1, m - 1)
        case (i, n, h, m)              => getNode(i, n(0).Left,  h - 1, m - 1)
    }
    private val changeNode: (Int, Double, List[Node], Int) => List[Node] = {
        case (i, x, n, 0) => {
            List(new Node(x))
        }
        case (i, x, n, h) if(wR(i, h)) => {
            val NR = changeNode(i, x, n(0).Right,  h - 1)
            List(new Node(n(0).Left, NR, F(n(0).Left(0).value, NR(0).value)))
        }
        case (i, x, n, h) => {
            val NL = changeNode(i, x, n(0).Left,  h - 1)
            List(new Node(NL, n(0).Right, F(NL(0).value, n(0).Right(0).value)))
        }
    }
    // private val printSpaces: Int => Unit = {
        // case 0 => ()
        // case x => {
            // print(" ")
            // printSpaces(x - 1)
        // }
    // }
    private val printNode: (Int, Int, Int, Int) => Unit = {
        (i, h, c, l) => {
            val N = getNode(i, Root, Height, h)
            print("[" + N(0).value + "] ")
            val step = math.pow(2, Height - h).toInt
            if (c + 1 < l) printNode(i + step, h, c + 1, l)
        }
    }
    private val printStage: (Int, Int, String) => Unit = {
        (h, l, S) => {
            //printSpaces(((math.pow(2, Height - h) - 1) * 1).toInt)
            print(S)
            printNode(0, h, 0, l)
            println()
            if (h + 1 <= Height) printStage(h + 1, l * 2, S)
        }
    }
    private val addList: (List[Double], SegmentTree) => SegmentTree = {
        case (Nil, tree)   => tree
        case (x::xs, tree) => addList(xs, tree.add(x))
    }
    private val calcNode: (Interval, Interval, List[Node]) => Double = {
        case (i, r, n) if (i == r) => {
            n(0).value
        }
        case (i, r, n) => {
            val S = r.split(r.Mid)
            val L = S(0).has(i)
            val R = S(1).has(i)
            (L, R) match{
                case (true, _) => calcNode(i, S(0), n(0).Left)
                case (_, true) => calcNode(i, S(1), n(0).Right)
                case (_, _) => {
                    val I = i.split(r.Mid)
                    val A = calcNode(I(0), S(0), n(0).Left)
                    val B = calcNode(I(1), S(1), n(0).Right)
                    F(A, B)
                }
            }
        }
    }
    
    def get(i: Int) : Double = {
        if (i > Length - 1) e
        else{
            val N = getNode(i, Root, Height, Height)
            N(0).value
        }
    }
    def change(i: Int, x: Double) : SegmentTree = {
        if (i > Capacity - 1) this
        else{
            val N = changeNode(i, x, Root, Height)
            new SegmentTree(N, Length, Capacity, Height)
        }
    }
    def add(A: List[Double]) : SegmentTree = {
        addList(A, this)
    }
    def add(a: Double) : SegmentTree = {
        if (Length < Capacity) {
            val N = changeNode(Length, a, Root, Height)
            new SegmentTree(N, Length + 1, Capacity, Height)
        }
        else {
            val E = crEST(Height)
            val N = new Node(Root, E, Root(0).value)
            val C = changeNode(Length, a, List(N), Height + 1)
            new SegmentTree(C, Length + 1, Capacity * 2, Height + 1)
        }
    }
    def calculate(a: Int, b: Int) : Double = {
        calcNode(new Interval(a, b), new Interval(0, Capacity), Root)
    }
    private val S = ""// + "x" * 60
    private val Z = ""// + "x "
    def printTree() = {
        println(S)
        println(Z + "Length: " + Length)
        println(Z + "Capacity: " + Capacity)
        println(Z + "Height: " + Height)
        println(Z + "Tree:")
        printStage(0, 1, Z)
        println(S + "\n")
    }
    def printArray() = {
        println(S)
        printStage(Height, Length, Z)
        println(S + "\n")
    }
}

object Main {
    def main(args: Array[String]) = {
        val Q = "\n" + "#" * 99 + "\n"
        
        println(Q + "1. TESTING CREATION" + Q)
        val a0 = new SegmentTree
        val a1 = a0.add(List[Double](3, 1, 1, 5, 6, 3, 1, 9, 8, 6, 7, 9))
        a1.printTree()
        a1.printArray()
            
        println(Q + "2. TEST GETTING" + Q)
        val i = 2
        println("Tree[" + i + "] == " + a1.get(i) + "\n")

        println(Q + "3. TESTING CALCULATION" + Q)
        val a = 7
        val b = 10
        println("F(" + a + ", ..., " + b + ") == " + a1.calculate(a, b) + "\n")

        println(Q + "4. TEST CHANGING" + Q)
        a1.printTree()
        val j= 3
        val n = 50
        val a2 = a1.change(j, n)
        println("Tree[" + j + "] = " + n)
        a2.printTree()

        println(Q)
        
        // /*private constructor required*/
        // val N30 = List(new Node(100))
        // val N31 = List(new Node(101))
        // val N32 = List(new Node(102))
        // val N33 = List(new Node(103))
        // val N34 = List(new Node(104))
        // val N35 = List(new Node(105))
        // val N36 = List(new Node(106))
        // val N37 = List(new Node(107))
        // val N20 = List(new Node(N30, N31, 100))
        // val N21 = List(new Node(N32, N33, 102))
        // val N22 = List(new Node(N34, N35, 104))
        // val N23 = List(new Node(N36, N37, 106))
        // val N10 = List(new Node(N20, N21, 100))
        // val N11 = List(new Node(N22, N23, 104))
        // val N00 = List(new Node(N10, N11, 100))
        // val a = new SegmentTree(N00, 0, 8, 3)
        // a.printTree()
        // println()
    }
}
