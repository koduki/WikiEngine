// vim: set ts=4 sw=4 et:
package cn.orz.pascal.wikiengine

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

class Document
class Inline extends Document
case class Text(s:String) extends Inline
case class Strong(t:Text) extends Inline
case class Line(l:List[Inline]) extends Document
case class Headline(t:List[Inline]) extends Document
case class Preformatted(s:String) extends Document
case class Paragraph(p:List[Document]) extends Document
case class Sentences(s:List[Document]) extends Document

object WikiParser extends Parsers {
    type Elem = Char
    def str(s: String): Parser[String] = if(s.length > 0) 
                                            elem(s.charAt(0)) ~ str(s.substring(1)) ^^ { case a ~ b => a + b }
                                         else success("")

    lazy val ASTRISK = elem('*')
    lazy val QUOT = elem('"')
    lazy val LF = elem('\n')
    lazy val TAB = elem('\t')

    lazy val PRE_OPEN = str("<pre>")
    lazy val PRE_CLOSE = str("</pre>")

    lazy val EOF = elem("EOF", _ == CharSequenceReader.EofCh)
    lazy val char:Parser[Char] = elem("ANY",  c => c != CharSequenceReader.EofCh)

    def sentences:Parser[Sentences] = rep1( preformatted | headline | paragraph ) ~ EOF ^^ { xs => Sentences(xs._1)}

    def paragraph:Parser[Paragraph] = rep1(line) ~ rep(LF) ^^ { xs => Paragraph(xs._1) }
    def headline:Parser[Headline] = ASTRISK ~ rep1(text | inline) ~ LF ^^ { xs => Headline(xs._1._2) }
    def preformatted:Parser[Preformatted] = PRE_OPEN ~ rep1(not(PRE_CLOSE) ~> char) ~ PRE_CLOSE ^^ { 
        case s ~ cs ~ e => Preformatted( cs.mkString ) 
    }

    def line: Parser[Line] = rep1(text | inline) ~ LF ^^ { xs => Line(xs._1)}

    def inline:Parser[Inline] = strong
    def strong:Parser[Strong] = QUOT ~ text ~ QUOT ^^ {xs => Strong(xs._1._2)}
    def text: Parser[Text] = rep1(not(LF) ~> not(QUOT) ~> not(ASTRISK) ~> char) ^^ {cs => Text(cs.mkString)}
}
