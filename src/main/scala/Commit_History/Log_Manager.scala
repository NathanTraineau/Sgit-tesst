package Commit_History

import java.io.File

import Create.Repository
import IO.{File_Tools, Output}
import Local_Changes.{Blob_Manager, Change, Diff_Manager, Set_Relationship}


case class Log(commit_name : String,
               commit_sha1 : String,
               commit_date : String,
               set_Relationship: Set_Relationship )


class Log_Manager(repo : Repository){
  //Get the las commit of the current branch

  def get_logs(commit_file : File,
               log_list : List[Log] = List[Log](),
               read_in_file_function : (File) => List[String] = File_Tools.read_in_file,
               diff : Diff_Manager = repo.get_diff_manager()
                ): List[Log] = {
    //This function take one commit as parameter and get the log information from each at each time
    val parse_commit : List[String] = read_in_file_function(commit_file)
    val commit_sha1 = parse_commit(0)
    val commit_parent_sha1 = parse_commit(1)
    val commit_name = parse_commit(2)
    val commit_date = parse_commit(3)
    val commit_index : List[String] = parse_commit.slice(4,parse_commit.length)

    if(commit_parent_sha1 == "0" *40){
      //When we arrive at a commit that doesn't have any parent we finish the log search
      val set_Relationship: Set_Relationship = diff.check_files_status(commit_index,List[String]())
      val log : Log = Log(commit_name, commit_sha1 , commit_date, set_Relationship )
      log_list.appended(log)
    }
    else {
      val commit_parent_file: File = repo.get_object_file_from_sha1(commit_parent_sha1, "commit")
      val parse_commit_parent = read_in_file_function(commit_parent_file)
      val commit_parent_index :List[String] = parse_commit_parent.slice(4, parse_commit_parent.length)
      val set_Relationship: Set_Relationship = diff.check_files_status(commit_index, commit_parent_index)
      val log: Log = Log(commit_name, commit_sha1, commit_date, set_Relationship)
      get_logs(commit_parent_file, log_list.appended(log))
    }
  }

  def log_toString(logs : List[Log], str : String = ""): String = {
      if (logs.isEmpty){
        str
      }else{
        val log = logs.head
        log_toString(logs.tail, str + "\ncommit" + log.commit_sha1 +  "\nDate :    " + log.commit_date +  "\nName :    " + log.commit_name +"\n")
      }
  }

  def log_p_toString(logs : List[Log], str : String = ""): String = {
    if (logs.isEmpty){
      str
    }else{
      val log = logs.head
      log_toString(logs.tail, str + "\ncommit" + log.commit_sha1 +  "\nDate :    " + log.commit_date +  "\nName :    " + log.commit_name +"\n" + Diff_Manager.diff_toString(log.set_Relationship) +"\n" +"\n" )
    }
  }

  def log_stat_toString(logs : List[Log], str : String = ""): String = {
    //This function make the
    @scala.annotation.tailrec
    def diff_toString_log(set_Relationship: Set_Relationship,
                          insertion : Int = 0,
                          deletion : Int = 0,
                          str : String = "",
                          get_blob_working_tree_file_path_function : (File, (File) => List[String]) => String = Blob_Manager.get_blob_working_tree_file_path,
                          read_in_file_function : (File) => List[String] = File_Tools.read_in_file,
                          count_type_function : (List[Change],Int,Int ) => List[Int] = Diff_Manager.count_type
                     ): String={
      if(set_Relationship.modified.nonEmpty) {
        val relationship = set_Relationship.modified.head
        val name = relationship.new_blob_file match {
          case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
          case None => ""
        }
        val modif_file = relationship.list_changes
        val modif_to_string = count_type_function(modif_file,0,0)
        val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified.tail,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted)
        diff_toString_log(new_set, insertion + modif_to_string(0), deletion + modif_to_string(1), str + name  + "     |   " + (modif_to_string(0) + modif_to_string(1))
          + "  " + "+" * modif_to_string(0) + "-" * modif_to_string(1) + "\n")
      }else
      if(set_Relationship.created.nonEmpty) {
        val relationship = set_Relationship.created.head
        val name = relationship.new_blob_file match {
          case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
          case None => ""
        }
        val modif_file = relationship.list_changes
        val modif_to_string = count_type_function(modif_file,0,0)
        val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created.tail,set_Relationship.deleted)
        diff_toString_log(new_set, insertion + modif_to_string(0), deletion + modif_to_string(1), str + name  + "     |   " + (modif_to_string(0) + modif_to_string(1))
          + "  " + "+" * modif_to_string(0) + "-" * modif_to_string(1) + "\n")
      }else
      if(set_Relationship.deleted.nonEmpty) {
        val relationship = set_Relationship.deleted.head
        val name = relationship.new_blob_file match {
          case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
          case None => ""
        }
        val modif_file = relationship.list_changes
        val modif_to_string = count_type_function(modif_file,0,0)
        val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted.tail)
        diff_toString_log(new_set, insertion + modif_to_string(0), deletion + modif_to_string(1), str + name  + "     |   " + (modif_to_string(0) + modif_to_string(1))
          + "  " + "+" * modif_to_string(0) + "-" * modif_to_string(1) + "\n")
      }else
      if(set_Relationship.renamed.nonEmpty) {
        val relationship = set_Relationship.renamed.head
        val name_new_blob = relationship.new_blob_file match {
          case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
          case None => ""
        }
        val name_former = relationship.former_Blob_File match {
          case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
          case None => ""
        }
        val modif_file = relationship.list_changes
        val modif_to_string = count_type_function(modif_file,0,0)
        val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed.tail,set_Relationship.created,set_Relationship.deleted)
        diff_toString_log(new_set, insertion + modif_to_string(0), deletion + modif_to_string(1), str + name_former  + " => "  + name_new_blob + "      |   " + (modif_to_string(0) + modif_to_string(1))
          + "  " + "+" * modif_to_string(0) + "-" * modif_to_string(1) + "\n")
      }else {
        //We return the string
        (str + str.split("\n").length + " files changed, " + insertion + " insertion(s)(+), " + deletion + " deletion(s)(-)").toString
      }
    }


    if (logs.isEmpty){
      str
    }else{
      val log = logs.head
      return log_stat_toString(logs.tail, str + "\ncommit " + log.commit_sha1 +  "\nDate :    " + log.commit_date +  "\nName :    " + log.commit_name +"\n" + diff_toString_log(log.set_Relationship))
    }
  }

}

object Log_Manager {

  def main(arg : String): Unit = {
    Repository.get_repo_path() match {
      case Some(value) => {
        val repo: Repository = new Repository(value)
        val log_manager = new Log_Manager(repo)
        val branch = repo.get_branch_manager()
        val last_commit_sha1 = branch.get_current_commit_sha1()
	if(last_commit_sha1 == "0"*40){
	      val commit_index =  "" :: Nil
	      Output.print_sgit("Your current branch does not have any commits yet")
	    }else {
        val commit_file: File = repo.get_object_file_from_sha1(last_commit_sha1, "commit")
        val logs: List[Log] = log_manager.get_logs(commit_file)
        arg match {
          case "" => Output.print_sgit(log_manager.log_toString(logs))
          case "p"  => Output.print_sgit(log_manager.log_p_toString(logs))
          case "stat" => Output.print_sgit(log_manager.log_stat_toString(logs))
        }
	}
      }
      case None =>
    }
  }
}
