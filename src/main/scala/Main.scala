import IO.Command_Parser

object Main {
  def main(args: Array[String]): Unit = {
        val rep = scala.io.StdIn.readLine()
        Command_Parser.parser(rep);
  }


}
