package Local_Changes

import java.io.File

import Commit_History.Log_Manager
import Create.Repository
import IO.{File_Tools, Output}
import Local_Changes.Diff_Manager.diff_toString

object Status_Manager {


  def main(): Unit = {
    Repository.get_repo_path() match {
      case Some(value) => {
        val repo: Repository = new Repository(value)
        val diff_class = new Diff_Manager(repo)
        //We get the index from the last commit file
        val log_manager = new Log_Manager(repo)
        val commit_index_lines = get_index_lines(repo)
        val index_Manager = new Index_Manager(repo)
        val index = index_Manager.get_index_lines()
        val set_Relationship_commit_stage : Set_Relationship = diff_class.get_set_relationship_static(index,commit_index_lines)
        val commit_stage_string : String = status_toString(set_Relationship_commit_stage)
        val working_tree_files = File_Tools.get_dirs_files_from_string(s"${repo.get_repo_path()}" :: Nil): List[File]
        val blob_Manager: Blob_Manager = repo.get_blob_manager()
        val blob_list = blob_Manager.blobs_create(working_tree_files)
        val blob_list_lines = blob_Manager.get_blob_list_index_line(blob_list)
        val set_Relationship_stage_untracked : Set_Relationship = diff_class.get_set_relationship_static(blob_list_lines,index)
        val stage_untracked_string : String = status_toString(set_Relationship_stage_untracked)
        Output.print_sgit("Here is the change to be comitted \n"+ commit_stage_string + " \n Here are the untracked changes \n" + stage_untracked_string)
      }
      case _ => None
    } //vreve
  }


  def get_index_lines(repo: Repository) : List[String] = {
    val branch = repo.get_branch_manager()
    val last_commit_sha1 = branch.get_current_commit_sha1()
    val commit_index = ""
    if(last_commit_sha1 == "0"*40){
      val commit_index =  "" :: Nil
      List[String]()
    }else {
      val commit_file: File = repo.get_object_file_from_sha1(last_commit_sha1, "commit")
      val parse_commit: List[String] = File_Tools.read_in_file(commit_file)
      val commit_index: List[String] = parse_commit.slice(4, parse_commit.length)
      commit_index
    }
  }

  def status_toString(set_Relationship: Set_Relationship, str : String ="",
                    get_blob_working_tree_file_path_function : (File,(File) => List[String]) => String = Blob_Manager.get_blob_working_tree_file_path,
                    read_in_file_function : (File) => List[String] = File_Tools.read_in_file): String={
    //if modified. pas empty :
    // et apreès on passe au suivant created...
    if(set_Relationship.modified.nonEmpty) {
      val relationship = set_Relationship.modified.head
      val name = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${relationship.new_Blob_file_name} has been modified \n"
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified.tail,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted)
      diff_toString(new_set, str + modified_introduction + "\n")
    }else
    if(set_Relationship.created.nonEmpty) {
      val relationship = set_Relationship.created.head
      val name = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been created \n"
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created.tail,set_Relationship.deleted)
      diff_toString(new_set, str + modified_introduction +  "\n")
    }else
    if(set_Relationship.deleted.nonEmpty) {
      val relationship = set_Relationship.deleted.head
      val name = relationship.former_Blob_File match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been deleted \n"
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted.tail)
      diff_toString(new_set, str + modified_introduction +  "\n")
    }else
    if(set_Relationship.renamed.nonEmpty) {
      val relationship = set_Relationship.renamed.head
      val name_new_blob = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val name_former_blob = relationship.former_Blob_File match {
        case Some(file) => relationship.index_line.split(" ")(1)
        case None => ""
      }
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed.tail,set_Relationship.created,set_Relationship.deleted)
      diff_toString(new_set, str +  name_former_blob  + " has been renamed into => "  + name_new_blob +  "\n")

    }else {
      //We return the string
      str
    }
  }

}
