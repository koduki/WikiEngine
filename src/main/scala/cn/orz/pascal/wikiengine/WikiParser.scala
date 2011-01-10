// vim: set ts=4 sw=4 et:
package cn.orz.pascal.wikiengine

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

class Document
class Inline extends Document
case class Text(str:String) extends Inline
case class Strong(text:Text) extends Inline
case class Link(text:Text, uri:Text) extends Inline
case class Line(line:List[Inline]) extends Document
case class Headline(text:List[Inline]) extends Document
case class Preformatted(str:String) extends Document
case class Paragraph(paragraph:List[Document]) extends Document
case class Sentences(sentences:List[Document]) extends Document

object WikiParser extends Parsers {
    type Elem = Char
    def str(s: String): Parser[String] = if(s.length > 0) 
                                            elem(s.charAt(0)) ~ str(s.substring(1)) ^^ { case a ~ b => a + b }
                                         else success("")

    lazy val ASTRISK = elem('*')
    lazy val QUOT = elem('"')
    lazy val LF = elem('\n')
    lazy val TAB = elem('\t')
    lazy val PIPE = elem('|')
    lazy val BS = elem('[')
    lazy val BE = elem(']')

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

    def inline:Parser[Inline] = strong | link
    def link:Parser[Link] = BS ~ BS ~ rep(text ~ PIPE) ~ text ~ BE ~ BE ^^ { 
        case b1 ~ b2 ~ List(text ~ b3) ~ uri ~ b4 ~ b5 => Link(text, uri)
        case b1 ~ b2 ~ uri ~ b3 ~ b4 => Link(uri, uri) 
    }
    def strong:Parser[Strong] = QUOT ~ text ~ QUOT ^^ {xs => Strong(xs._1._2)}
    def text: Parser[Text] = rep1(not(LF) ~> not(QUOT) ~> not(ASTRISK) ~> not(BS) ~> not(BE) ~> not(PIPE) ~> char) ^^ {cs => Text(cs.mkString)}
}
