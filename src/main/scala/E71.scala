import scala.annotation.tailrec

object E71 {
  def simplifyPath(path: String): String = "/" + simplifyPath(path, Nil).reverse.mkString("/")

  @tailrec
  private def simplifyPath(remainingPath: String, currentPath: List[String]): List[String] =
    remainingPath match {
      case s"/$rest" => simplifyPath(rest, currentPath)
      case s"./$rest" => simplifyPath(rest, currentPath)
      case "." => currentPath
      case s"../$rest" => simplifyPath(rest, if (currentPath.isEmpty) Nil else currentPath.tail)
      case ".." => if (currentPath.isEmpty) Nil else currentPath.tail
      case "" => currentPath
      case s"$catalog/$rest" => simplifyPath(rest, catalog :: currentPath)
      case s"$catalog" => catalog :: currentPath
    }


  def main(args: Array[String]): Unit = {
    println(simplifyPath("/home/") == "/home")
    println(simplifyPath("/../") == "/")
    println(simplifyPath("/home//foo/") == "/home/foo")
    println(simplifyPath("/home//foo/..//foo") == "/home/foo")
  }

}
