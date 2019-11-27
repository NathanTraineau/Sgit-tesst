package IO

import java.awt.print.Printable
import java.io.File

import Branches.Branch_Manager
import Commit_History.Log_Manager
import Create.Repository
import Local_Changes.{Add_Manager, Commit_Manager, Diff_Manager, Status_Manager}

case class Command_Parsed(command_name : String, argument : String, list_string : List[String])

object Command_Parser {

  def parser(input: String): Unit = {
    val input_list = input.split(" ")
        // We know that the second input after sgit is the command_name
        val cmd = Command_Parsed(input_list(0),null,List[String]())

        val (_,body) = input_list.splitAt(1)



        def parse_body(command_Parsed: Command_Parsed, body: List[String]): Command_Parsed = {
            // We define the pattern that represents arguments
            val arg_pattern = """\-(.*)""".r
            val arg2_pattern = """\--(.*)""".r

            //We fullfill the command_Parsed with the right informations
            body match {
              case Nil => command_Parsed

              case arg2_pattern(arg) :: tail => val cp = command_Parsed.copy(command_Parsed.command_name, arg, command_Parsed.list_string)
                parse_body(cp, tail)

              case arg_pattern(arg) :: tail => val cp = command_Parsed.copy(command_Parsed.command_name, arg, command_Parsed.list_string)
                parse_body(cp, tail)

              case str :: tail =>
                if (str.contains("/")){
                  val str_interm = str.split("/")
                  val new_str = str_interm.mkString(File.separator)
                  val cp = command_Parsed.copy(command_Parsed.command_name, command_Parsed.argument, command_Parsed.list_string.appended(new_str) )
                  parse_body(cp, tail)
                }else{
                  val cp = command_Parsed.copy(command_Parsed.command_name, command_Parsed.argument, command_Parsed.list_string.appended(str) )
                  parse_body(cp, tail)
                }

              case _ => command_Parsed
            }
          }

//freerv

        val command_parsed = parse_body(cmd, body.toList)
        command_parsed match {
        case Command_Parsed("init",null,List())  => Repository.initialize_repo(System.getProperty("user.dir"))
        case  Command_Parsed("init",null,infos)   => Repository.initialize_repo(System.getProperty("user.dir") + {File.separator} + infos(0))
        case Command_Parsed("add",null,infos)  => if(infos.nonEmpty){Add_Manager.main(infos)}
        case Command_Parsed("diff", null, List()) => Diff_Manager.main
        case Command_Parsed("commit", "m", commit_name) => Commit_Manager.main(commit_name.mkString(" "))
        case Command_Parsed("commit",null, List()) => println("Please type the command commit -m {commit_name} to make a commit")
        case Command_Parsed("log",arg,List()) => if (arg == null) {
          Log_Manager.main("")
        } else if (arg == "p" || arg == "stat") {Log_Manager.main(arg)}
        case Command_Parsed("branch",null,infos) => if(infos.nonEmpty){Branch_Manager.main(infos(0).toString.split("/").mkString(("")))}
        case Command_Parsed("branch","av",List()) => Branch_Manager.main_av()
        case Command_Parsed("status",null,List()) => Status_Manager.main()
        case _ => println("Unknown command")

        }
    }
}
