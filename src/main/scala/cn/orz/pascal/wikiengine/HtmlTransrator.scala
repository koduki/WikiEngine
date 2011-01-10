// vim: set ts=4 sw=4 et:
package cn.orz.pascal.wikiengine

object HtmlTransrator {
    def parse(doc:Document):String = {
        doc match {
            case Text(s) => s  
            case Strong(t) => "<strong>" + parse(t) + "</strong>"
            case Link(text, url) => "<a href='" + parse(url)  + "'>" + parse(text) + "</a>"
            case Line(l) => l.map( t => parse(t)).mkString + "<br />"
            case Headline(l) => "<h1>" + l.map( t => parse(t)).mkString + "</h1>"
            case Preformatted(text) => "<pre>" + text + "</pre>"
            case Paragraph(p) => "<p>" + p.map(xs => parse(xs)).mkString + "</p>"
            case Sentences(s) => "<article>" + s.map(xs => parse(xs)).mkString + "</article>"
        }  
    }
}
