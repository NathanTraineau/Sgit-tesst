package IO

import scopt.OParser

import Branches.Branch_Manager
import Commit_History.Log_Manager
import Create.Repository
import Local_Changes.{Add_Manager, Commit_Manager, Diff_Manager, Status_Manager}

object Command_Parser {

  def main(args: Array[String]): Unit = {

    case class Config(
                       command: String = "",
                       files: List[String] = List(),
                       branch_tag: String = "",
                       commitMessage: List[String] = List(),
                       pLog: Boolean = false,
                       av: Boolean = false)

    val builder = OParser.builder[Config]


    val parser1 = {
      import builder._
      OParser.sequence(
        programName("sgit"),
        head("scopt", "4.x"),
        help("help").text("List of all commands."),
        cmd("init")
          .text("Initialize the repository.")
          .action((_, c) => c.copy(command = "init")),

        cmd("status")
          .text("Working directory status.")
          .action((_, c) => c.copy(command = "status")),

        cmd("diff")
          .text("Changes between working directory and stage area, working directory and commit.")
          .action((_, c) => c.copy(command = "diff")),

        cmd("log")
          .text("Show all commits started with newest.")
          .action((_, c) => c.copy(command = "log"))
          .children(
            opt[Unit]('p', "patchLog")
              .optional()
              .action((_, c) => c.copy(pLog = true))
              .text("Show changes overtime.")
          ),

        cmd("add")
          .text("Add files to the stage area.")
          .action((_, c) => c.copy(command = "add"))
          .children(
            arg[String]("<file> or <files> or .")
              .unbounded
              .required
              .action((x, c) => c.copy(files = c.files :+ x))
              .text("Files to add.")
          ),

        cmd("commit")
          .text("Save changes to the repository.")
          .action((_, c) => c.copy(command = "commit"))
          .children(
            opt[String]('m', "message")
              .required
              .action((x, c) => c.copy(commitMessage = c.commitMessage :+ x))
              .text("Message to commit.")
          ),

        cmd("branch")
          .text("Create branch or list branches and tags.")
          .action((_, c) => c.copy(command = "branch"))
          .children(
            arg[String]("name")
              .optional()
              .action((x, c) => c.copy(branch_tag = x))
              .text("Name of the branch."),
            opt[Unit]("av")
              .action((_, c) => c.copy(av = true))
              .text("Display all the branches and tags.")
          ),

        cmd("checkout")
          .text("Branch switching.")
          .action((_, c) => c.copy(command = "checkout"))
          .children(
            arg[String]("name")
              .required()
              .action((x, c) => c.copy(branch_tag = x))
              .text("Name of the branch."),
          ),

        cmd("tag")
          .text("Create a tag.")
          .action((_, c) => c.copy(command = "tag"))
          .children(
            arg[String]("name")
              .required
              .action((x, c) => c.copy(branch_tag = x))
              .text("Name of the tag.")
          ),

      )
    }

    OParser.parse(parser1, args, Config()) match {
      case Some(config) => {
        config.command match {
          case "init" => {
            if (Repository.repo_exists()) {
              println("Already init.")
            }
            else {
              Repository.initialize_repo(System.getProperty("user.dir"))
            }
          }
          case _ => {
            if (Repository.repo_exists()) {
              config.command match {

                case "add" => {
                  if (config.files.nonEmpty) {
                    Add_Manager.main(config.files)
                  }
                }
                case "status" => {
                  Status_Manager.main()
                }
                case "branch" => {
                  if (config.av) {
                    Branch_Manager.main_av()
                  } else {
                    if (config.branch_tag.nonEmpty) {
                      Branch_Manager.main(config.branch_tag(0).toString.split("/").mkString(("")))
                    }
                  }
                }
                case "commit" => {
                  Commit_Manager.main(config.commitMessage.mkString(" "))
                }
                case "diff" => {
                  Diff_Manager.main
                }
                case "log" => {
                  if (config.pLog) {
                    print("logpppp")
                    Log_Manager.main("p")
                  } else {
                    Log_Manager.main("")
                  }
                }
                case _ => {
                  // arguments are bad
                }
              }
            }
            else {
              Output.print_sgit("No repository found. Please, initialize one.")
            }
            // arguments are bad, error message is displayed
          }
        }
      }
      case _ => {
        // arguments are bad
      }
    }
  }
}
