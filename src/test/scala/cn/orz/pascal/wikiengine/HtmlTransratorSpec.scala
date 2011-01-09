// vim: set ts=4 sw=4 et:
package cn.orz.pascal.wikiengine
import org.specs._

object HtmlTransratorSpec extends Specification {
    "Text is plane text." in {
        val ast = Text("text") 
        HtmlTransrator.parse(ast) must_== "text"
    }

    "Strong is strong tag." in {
        val ast = Strong(Text("strong text")) 
        HtmlTransrator.parse(ast) must_== "<strong>strong text</strong>"
    }

    "Line eol is br tag." in {
        val ast = Line(List(Text("text"))) 
        HtmlTransrator.parse(ast) must_== "text<br />"
    }

    "Line contain any inlines." in {
        val ast = Line(List(Text("Hello"), Strong(Text("World")))) 
        HtmlTransrator.parse(ast) must_== "Hello<strong>World</strong><br />"
    }

   "headline is h1 tag." in {
        val ast = Headline(List(Text("Title1."))) 
        HtmlTransrator.parse(ast) must_== "<h1>Title1.</h1>"
    }

   "paragraph is p tag." in {
        val ast = Paragraph(List(Line(List(Text("hello world."))))) 
        HtmlTransrator.parse(ast) must_== "<p>hello world.<br /></p>"
    }

   "paragraph contain any line." in {
        val ast = Paragraph(List(Line(List(Text("hello world."))), 
                                 Line(List(Text("I "), Strong(Text("love")), Text(" Scala."))))) 
        HtmlTransrator.parse(ast) must_== "<p>hello world.<br />I <strong>love</strong> Scala.<br /></p>"
    }

    "sentences is article tag." in {
        val ast = Sentences(List(Headline(List(Text("Title"))),
                                Paragraph(List(Line(List(Text("hello"))))),
                                Paragraph(List(Line(List(Text("world")))))))    
        HtmlTransrator.parse(ast) must_== "<article><h1>Title</h1><p>hello<br /></p><p>world<br /></p></article>"
    }
}
