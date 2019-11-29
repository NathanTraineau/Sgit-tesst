package Local_Changes

import java.io.File

import Create.Repository
import IO.{Date_Tool, File_Tools, Output}
import Local_Changes.Diff_Manager.{count_type, diff_toString, local_diff_toString}

import scala.io.Source

case class Commit(name : String, blob_lines_from_index : List[String], commit_parent_sha1: String){
  val sha1 : String = File_Tools.calulate_sha1(blob_lines_from_index.mkString("") + commit_parent_sha1 + name)
  val dir_name : String = sha1.slice(0,2)
  val file_name : String = sha1.slice(2,sha1.length)
  val blob_lines : List[String] = Commit_Manager.withdraw_deleted_files_from_blob_lines(blob_lines_from_index)
}

class Commit_Manager(repo : Repository){
  def create_commit_file(commit: Commit, write_file_function : (File,String)  => Unit = File_Tools.write_in_file,
                         date : String = Date_Tool.get_Date_As_String(),
                         repo_sgit_path : String = repo.get_sgith_path()): File = {
    //We do a callback in order to keep the function as a first citizen
    val real_path_dir = s"$repo_sgit_path${File.separator}commit${File.separator}${commit.dir_name}"
    val real_path_file = s"$repo_sgit_path${File.separator}commit${File.separator}${commit.dir_name}${File.separator}${commit.file_name}"
    new File(real_path_dir).mkdir()
    val commit_file = new File(real_path_file)
    commit_file.createNewFile()
    write_file_function(commit_file, commit.sha1 + "\n" + commit.commit_parent_sha1 + "\n" + commit.name + "\n" +   date   + "\n" + commit.blob_lines.mkString("\n"))
    commit_file
  }
}
object Commit_Manager {
  def main(name : String): Unit = {
    Repository.get_repo_path() match {
      case Some(value) =>
        val repo: Repository = new Repository(value)
        val repo_path = value
        val commit_manager = repo.get_commit_manager()
        val index_manager = repo.get_index_manager()
        val head_manager = repo.get_head_manager()
        val branch = repo.get_branch_manager() // fzfaf
        //First we get the current index file
        val index_lines = index_manager.get_index_lines()
        //Then we get the last commit of the current branch thanks to HEAD
        val current_commit_sha1 = branch.get_current_commit_sha1()
        //Then we create the commit with this informations
        //val current_commit : File = repo.get_object_file_from_sha1(current_commit_sha1,"commit")
        val commit = Commit(name, index_lines, current_commit_sha1)
        //Then we add the commit to the .sgit
        val diff = new Diff_Manager(repo)
        commit_manager.create_commit_file(commit)
        val set_Relationship = diff.get_set_relationship_static(commit.blob_lines,Status_Manager.get_index_lines(repo))
        //Then we reset the index ( we remove the deleted files)
        index_manager.reset_index(Some(commit.blob_lines))
        //We add it to the current branch and we update the HEAD
        branch.change_last_commit_branch(commit.sha1)
        head_manager.update_HEAD(commit.sha1)
        Output.print_sgit("Commit made on branch "+ branch.get_current_branch_name() + "\n")
        val diff_string : String = commit_toString(set_Relationship)
        Output.print_sgit(diff_string)
      case None =>
    }
  }

  def commit_toString(set_Relationship: Set_Relationship, str : String ="",
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
      val modified_introduction = s"${name} has been modified \n"
      val count_type_occ :List[Int] = count_type(relationship.list_changes)
      val changes = (  s"${count_type_occ(0)} inserted (+)  ${count_type_occ(1)} deleted (-) \n")
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified.tail,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted)
      commit_toString(new_set, str + changes + modified_introduction + "\n")
    }else
    if(set_Relationship.created.nonEmpty) {
      val relationship = set_Relationship.created.head
      val name = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been created \n"
      val count_type_occ :List[Int] = count_type(relationship.list_changes)
      val changes = (  s"${count_type_occ(0)} inserted (+)  ${count_type_occ(1)} deleted (-) \n")
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created.tail,set_Relationship.deleted)
      commit_toString(new_set, str + changes + modified_introduction +  "\n")
    }else
    if(set_Relationship.deleted.nonEmpty) {
      val relationship = set_Relationship.deleted.head
      val name = relationship.former_Blob_File match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been deleted \n"
      val count_type_occ :List[Int] = count_type(relationship.list_changes)
      val changes = (  s"${count_type_occ(0)} inserted (+)  ${count_type_occ(1)} deleted (-) \n")
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted.tail)
      commit_toString(new_set, str + changes + "\n" +  modified_introduction +  "\n")
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
      commit_toString(new_set, str +  name_former_blob  + " has been renamed into => "  + name_new_blob +  "\n")

    }else {
      //We return the string
      str
    }
  }
  @scala.annotation.tailrec
  def withdraw_deleted_files_from_blob_lines(blob_lines : List[String], new_blob_lines : List[String] = List[String]()): List[String] = {
      if(blob_lines.isEmpty){
        new_blob_lines
      }else
      if(blob_lines.head.split(" ").last == "deleted"){
        withdraw_deleted_files_from_blob_lines(blob_lines.tail,new_blob_lines)
      }else{
        withdraw_deleted_files_from_blob_lines(blob_lines.tail,new_blob_lines.appended(blob_lines.head))
      }
  }
}
