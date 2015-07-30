import scala.reflect.io.File
import scalaz._, Scalaz._
import scala.reflect.macros.blackbox
import scala.language.experimental.macros


object Path {

  type FilePath = String
  case class Path[B,T](f: FilePath)

  sealed trait Abs extends AnyRef
  sealed trait Rel extends AnyRef

  sealed trait File extends AnyRef
  sealed trait Dir extends AnyRef

  sealed trait PathParseException extends Exception
  case class InvalidAbsDir(f: FilePath) extends PathParseException
  case class InvalidRelDir(f: FilePath) extends PathParseException
  case class InvalidAbsFile(f: FilePath) extends PathParseException
  case class InvalidRelFile(f: FilePath) extends PathParseException
  case class CouldntStripPrefixDir(f: FilePath, l: FilePath) extends PathParseException


  implicit def eqPath[B,T] = new Equal[Path[B,T]] {
    def equal(p1: Path[B,T], p2: Path[B,T]): Boolean =
      p1.f === p2.f
  }

  implicit def orderPath[B,T] = new Order[Path[B,T]] {
    def order(p1: Path[B,T], p2: Path[B,T]): Ordering =
      p1.f ?|? p2.f
  }

  implicit def showPath[B,T] = new Show[Path[B,T]] {
    override def shows(p: Path[B,T]): String =
      "Path: " ++ p.f.shows
  }

  implicit class PathsAppendOps[B,T](val p1: Path[B,Dir]) extends AnyVal {
    def </>(p2: Path[Rel,T]): Path[B,T] =
      Path(p1.f ++ p2.f)
  }

  def toFilePath[B,T](p: Path[B,T]): FilePath =
    p.f

  def parseAbsDir[B,T](f: FilePath): \/[InvalidAbsDir, Path[Abs,Dir]] = {
    val path = scalax.file.Path.fromString(f)
    if (path.isAbsolute && path.isDirectory && !normalizeDir(f).isEmpty && !f.startsWith("~/") && !hasParentDir(f))
      Path[Abs, Dir](normalizeDir(f)).right[InvalidAbsDir]
    else
      InvalidAbsDir(f).left
  }

  def parseRelDir[B,T](f: FilePath): \/[InvalidRelDir, Path[Rel,Dir]] = {
    val path = scalax.file.Path.fromString(f)
    if (!path.isAbsolute && path.isDirectory && !f.isEmpty && !f.startsWith("~/") && !hasParentDir(f) && normalizeDir(f) != "" && f != "..")
      Path[Rel,Dir](normalizeDir(f)).right[InvalidRelDir]
    else
      InvalidRelDir(f).left
  }

  def parseAbsFile[B,T](f: FilePath): \/[InvalidAbsFile, Path[Abs,File]] = {
    val path = scalax.file.Path.fromString(f)
    if (path.isAbsolute && !path.isDirectory && !hasTrailingPathSeparator(f) && !f.startsWith("~/") && hasParentDir(f) && normalizeFile(f) != "" && f != "..")
      Path[Abs,File](normalizeFile(f)).right[InvalidAbsFile]
    else
      InvalidAbsFile(f).left
  }

  def parseRelFile[B,T](f: FilePath): \/[InvalidRelFile, Path[Rel,File]] = {
    val path = scalax.file.Path.fromString(f)
    if (!(path.isAbsolute || hasTrailingPathSeparator(f)) && !path.isDirectory && f != "" && !f.startsWith("~/") && !hasParentDir(f) && normalizeFile(f) != "" && f != "..")
      Path[Rel,File](normalizeFile(f)).right[InvalidRelFile]
    else
      InvalidRelFile(f).left
  }

  private def hasTrailingPathSeparator(f: FilePath): Boolean = {
    f.last == java.io.File.pathSeparatorChar
  }

  private def hasParentDir(f: FilePath): Boolean = {
    val filepath = java.io.File.pathSeparatorChar match {
      case '/' => f
      case  x  => f.map(y => if (x == y) '/' else y)
    }
    filepath.startsWith("../") || filepath.contains("/../") || filepath.endsWith("/..")
  }

  def parent[B,T](p: Path[Abs,T]): Path[Abs,Dir] = {
    val path: scalax.file.Path = scalax.file.Path.fromString(p.f)
    val parent = path.parent.getOrElse(path)
    Path(normalizeDir(parent.path))
  }

  def filename[B,T](p: Path[B,File]): Path[Rel,File] = {
    val path = scalax.file.Path.fromString(p.f)
    Path(normalizeFile(path.name))
  }

  def dirname[B,T](p: Path[B,Dir]): Path[Rel,Dir] = {
    val path = scalax.file.Path.fromString(p.f)
    Path(path.name)
  }

  private def clean(f: FilePath): FilePath = f.toArray match {
    case Array('.', '/') => ""
    case Array('/', '/', xs @ _*) => clean("/" ++ xs)
    case x => x.mkString
  }

  private def addTrailingPathSeparator(f: FilePath): FilePath =
    if (f.last.toString != File.separator) f ++ "/"
    else f

  private def normalize(f: FilePath): FilePath =
    scalax.file.Path.fromString(f).normalize.path

  private def normalizeDir(f: FilePath): FilePath =
    clean(addTrailingPathSeparator(normalize(f)))

  private def normalizeFile(f: FilePath): FilePath =
    clean(normalize(f))

  object mkAbsDir {
    def apply[B,T](f: FilePath): Path[Abs,Dir] = macro implMkAbsDir[B,T]
    def implMkAbsDir[B,T](c: blackbox.Context)(f: c.Expr[FilePath]) = {
      import c.universe._
      val x = c.eval(c.Expr[FilePath](c.untypecheck(f.tree.duplicate.asInstanceOf[c.Tree])))
      parseAbsDir(x) match {
        case -\/(error) => c.abort(c.enclosingPosition, "Invalid absolute directory.")
        case \/-(p) => c.Expr[Path[Abs,Dir]](q"""Path.Path(${p.f})""")
      }
    }
  }

  object mkRelDir {
    def apply[B,T](f: FilePath): Path[Rel,Dir] = macro implMkRelDir[B,T]
    def implMkRelDir[B,T](c: blackbox.Context)(f: c.Expr[FilePath]) = {
      import c.universe._
      val x = c.eval(c.Expr[FilePath](c.untypecheck(f.tree.duplicate.asInstanceOf[c.Tree])))
      parseRelDir(x) match {
        case -\/(error) => c.abort(c.enclosingPosition, "Invalid relative directory.")
        case \/-(p) => c.Expr[Path[Rel,Dir]](q"""Path.Path(${p.f})""")
      }
    }
  }

  object mkAbsFile {
    def apply[B,T](f: FilePath): Path[Abs,File] = macro implMkAbsFile[B,T]
    def implMkAbsFile[B,T](c: blackbox.Context)(f: c.Expr[FilePath]) = {
      import c.universe._
      val x = c.eval(c.Expr[FilePath](c.untypecheck(f.tree.duplicate.asInstanceOf[c.Tree])))
      parseAbsFile(x) match {
        case -\/(error) => c.abort(c.enclosingPosition, "Invalid absolute file.")
        case \/-(p) => c.Expr[Path[Abs,File]](q"""Path.Path(${p.f})""")
      }
    }
  }

  object mkRelFile {
    def apply[B,T](f: FilePath): Path[Rel,File] = macro implMkRelFile[B,T]
    def implMkRelFile[B,T](c: blackbox.Context)(f: c.Expr[FilePath]) = {
      import c.universe._
      val x = c.eval(c.Expr[FilePath](c.untypecheck(f.tree.duplicate.asInstanceOf[c.Tree])))
      parseRelFile(x) match {
        case -\/(error) => c.abort(c.enclosingPosition, "Invalid relative file.")
        case \/-(p) => c.Expr[Path[Rel,File]](q"""Path.Path(${p.f})""")
      }
    }
  }

}


object Main {

  import Path._

  def main(args: Array[String]) {
    println("Hellow")
  }
}