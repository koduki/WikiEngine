// vim: set ts=4 sw=4 et:
import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info) {
    val specs = "org.specs" % "specs" % "1.6.6" from "http://specs.googlecode.com/files/specs_2.8.1-1.6.6.jar"
}
