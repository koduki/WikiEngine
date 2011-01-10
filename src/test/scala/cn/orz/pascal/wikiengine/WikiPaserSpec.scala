// vim: set ts=4 sw=4 et:
package cn.orz.pascal.wikiengine
import org.specs._

object WikiParserSpec extends Specification {
    def $(text:String) = new scala.util.parsing.input.CharSequenceReader(text)

    "WikiParser" should {
        """ plane text is Text.""" in {
           WikiParser.text($("hello world")).get must_== Text("hello world") 
        }

        """ " is Strong.""" in {
           WikiParser.inline($("\"scala\"")).get must_== Strong(Text("scala"))
        }

        """ \n is Line.""" in {
           WikiParser.line($("I love \"scala\".\n")).get must_== Line(List(Text("I love "), Strong(Text("scala")), Text(".")))
        }
 
        """ * is Headline.""" in {
           WikiParser.headline($("*Headline\n")).get must_== Headline(List(Text("Headline")))
        }

        """ \n\n is Paragraph.""" in {
           WikiParser.paragraph($("I love \nscala.\n\n")).get must_== Paragraph(List(
                                                                        Line(List(Text("I love "))), 
                                                                        Line(List(Text("scala.")))))
        }

        """ [[url]] is Link.""" in {
           WikiParser.link($("[[http://foo.bar.com/]]")).get must_== Link(Text("http://foo.bar.com/"), Text("http://foo.bar.com/"))
        }

        """ [[text|url]] is Link with Text.""" in {
           WikiParser.link($("[[hoge|http://foo.bar.com/]]")).get must_== Link(Text("hoge"), Text("http://foo.bar.com/"))
        }

        """ <pre> is Preformated Text.""" in {
           WikiParser.preformatted($("<pre>I love \nscala.\n\n</pre>")).get must_== Preformatted("I love \nscala.\n\n")
        }

        """ all text is Sentences.""" in {
           WikiParser.sentences($("*Headline\nI \"love\" \n[[scala|http://scala.com]].\n\n<pre>Hello\n</pre>")).get must_== Sentences(List(
                                                                        Headline(List(Text("Headline"))),
                                                                        Paragraph(List(
                                                                        Line(List(Text("I "), Strong(Text("love")), Text(" "))), 
                                                                        Line(List(Link(Text("scala"), Text("http://scala.com")),Text("."))))),
                                                                        Preformatted("Hello\n")
                                                                        ))

        }

    }
}
