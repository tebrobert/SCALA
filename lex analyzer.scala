/* Заготовка лексического анализатора, дополненная трейтами, осуществляющими
 * распознавание лексем:
 * 
 * - комментарии (целиком строка текста, начинающаяся с "*")
 * - идентификаторы (либо последовательности латинских букв нечётной длины,
 *   либо последовательности символов "*")
 * - ключевые слова ("with", "end", "**")
 * 
 * Также реализовано восстановление при ошибках.
 */

class Pos private (val prog: String, val offs: Int, val line: Int, val col: Int){
    def this(prog: String) = this(prog, 0, 1, 1)
    def ch = if(offs == prog.length) -1 else prog(offs)
    def inc = ch match{
        case '\n'   => new Pos(prog, offs + 1, line + 1, 1)
        case -1     => this
        case _      => new Pos(prog, offs + 1, line, col + 1)
    }
    override def toString = "(" + line + ", " + col + ")"
}

object DomainTags extends Enumeration{
    type Tag = Value
    val WHITESPACE, COMMENT, KEYWORD, IDENT, END_OF_PROGRAM, ERROR = Value
}
import DomainTags ._

class Scanner {
    def scan(start: Pos ): (Tag, Pos) =
        sys.error("syntax error at " + start)
}

class Token(val start: Pos, scanner: Scanner){
    val (tag, follow) = start.ch match{
        case -1 => (END_OF_PROGRAM, start)
        case _  => scanner.scan(start)
    }
    def image = start.prog.substring(start.offs, follow.offs)
    def next = new Token(follow, scanner)
}


trait Idents extends Scanner {
    var error = false
    private def missLetter(pos: Pos): Pos = {
        if(pos.ch.toChar.isLetter) missLetter(pos.inc)
        else if(pos.ch=='*') {error=true; missLetter(pos.inc)}
        else pos
    }
    override def scan(start: Pos) = {
        if(start.ch.toChar.isLetter) {
            error = false
            val follow = missLetter(start)
            if((follow.offs - start.offs)%2!=1) error = true
            if(!error) (IDENT, follow)
            else (ERROR, follow)
        }
        else super.scan(start)
    }
}

trait SIdents extends Scanner {
    var Serror = false
    private def missStar(pos: Pos): Pos = {
        if(pos.ch == '*') missStar(pos.inc)
        else if(pos.ch.toChar.isLetter) {Serror=true; missStar(pos.inc)}
        else pos
    }
    override def scan(start: Pos) = {
        if(start.ch == '*') {
            Serror = false
            val follow = missStar(start)
            if(!Serror) (IDENT, follow)
            else (ERROR, follow)
        }
        else super.scan(start)
    }
}

trait Keywords extends Scanner {
    override def scan(start: Pos) = {
        if(
            start.offs+4 <= start.prog.length
            && start.prog.substring(start.offs, start.offs+4)=="with"
            && (start.offs+4+1 > start.prog.length
            || (start.prog(start.offs+4)!='*'
            && !start.prog(start.offs+4).isLetter))
        ){
            val follow = start.inc.inc.inc.inc
            (KEYWORD, follow)
        }
        else if(
            start.offs+3 <= start.prog.length
            && start.prog.substring(start.offs, start.offs+3)=="end"
            && (start.offs+3+1 > start.prog.length
            || (start.prog(start.offs+3)!='*'
            && !start.prog(start.offs+3).isLetter))
        ){
            val follow = start.inc.inc.inc
            (KEYWORD, follow)
        }
        else if(
            start.offs+2 <= start.prog.length
            && start.prog.substring(start.offs, start.offs+2)=="**"
            && (start.offs+2+1 > start.prog.length
            || (start.prog(start.offs+2)!='*'
            && !start.prog(start.offs+2).isLetter))
        ){
            val follow = start.inc.inc
            (KEYWORD, follow)
        }
        else super.scan(start)
    }
}

trait Comments extends Scanner {
    private def missComment(pos: Pos): Pos = {
        if (pos.ch != '\n' & pos.ch != -1) missComment(pos.inc)
        else pos
    }
    override def scan(start: Pos) = {
        if (start.col == 1 & start.ch == '*') {
            val follow = missComment(start)
            (COMMENT, follow)
        }
        else super.scan(start)
    }
}

trait Whitespaces extends Scanner{
    private def missWhitespace (pos: Pos): Pos = pos.ch match {
        case ' '    => missWhitespace(pos.inc)
        case '\t'   => missWhitespace(pos.inc)
        case '\n'   => missWhitespace(pos.inc)
        case _      => pos
    }
    override def scan(start: Pos) = {
        val follow = missWhitespace(start)
        if(start != follow) (WHITESPACE, follow)
        else super.scan(start)
    }
}

var t = new Token(
    new Pos("program \n *source goess here lol * ** *** ****\n*****\nwit with withh en end endd"),
    new Scanner
        with Idents
        with SIdents
        with Keywords
        with Comments
        with Whitespaces
)

while (t.tag != END_OF_PROGRAM){
    if(t.tag!=WHITESPACE)
    println(t.tag.toString + " " + t.start + "-" + t.follow + ": \"" + t.image + "\"")
    t = t.next
}
