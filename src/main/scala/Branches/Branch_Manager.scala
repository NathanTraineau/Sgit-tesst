package Branches

import java.io.File

import Create.Repository
import IO.{File_Tools, Output}
import Local_Changes.Head_Manager

case class Branch_Manager(repo : Repository){

  def create_branch_file(branch_name: String, current_commit : String = get_current_commit_sha1(),
                         write_file_function : (File,String)  => Unit = File_Tools.write_in_file,
                         repo_sgit_path : String = repo.get_sgith_path()): File = {
    //We do a callback in order to keep the function as a first citizen
    val real_path_file = s"${repo_sgit_path}${File.separator}branch${File.separator}$branch_name"
    val branch = new File(real_path_file)
    branch.createNewFile()
    write_file_function(branch,current_commit)
    branch
  }
  //fesgre

  def get_current_branch_name(last_commit_HEAD :String = get_current_commit_sha1(),
                              all_branch_names : List[String] = get_all_branch_names(),
                              get_branch_last_commit_function : (String, (File) => List[String]) => String = get_branch_last_commit,
                              read_in_file_function : (File) => List[String] = File_Tools.read_in_file): String ={
    //This function get all the branches and compare the sha1 with the one in the HEAD to get the name of the current branch
    //The sha 1 in the HEAD should be in one of the branch
    val sha1_branch = get_branch_last_commit_function(all_branch_names.head,read_in_file_function)
    if(sha1_branch == last_commit_HEAD ){
      all_branch_names.head
    }else{
      get_current_branch_name(last_commit_HEAD,all_branch_names.tail)
    }
  }

  def change_current_branch(branch_name : String,
                            write_in_file_function : (File,String) => Unit = File_Tools.write_in_file,
                            head_manager : Head_Manager = repo.get_head_manager(),
                            get_branch_last_commit_function : (String, (File) => List[String]) => String = get_branch_last_commit,
                            read_in_file_function : (File) => List[String] = File_Tools.read_in_file): Unit ={
    //Write into the head file the current branch name and its last commit
    val branch_sha1 = get_branch_last_commit_function(branch_name,read_in_file_function)
    val head = head_manager.get_HEAD_file()
    head.delete()
    val new_current = new File(s"${repo.get_sgith_path()}${File.separator}HEAD")
    new_current.createNewFile()
    write_in_file_function(new_current,branch_sha1)
  }

  def get_branch_file(branch_name : String, get_local_file_function : (String) => File = repo.get_local_file ): File = {
    get_local_file_function(s"branch${File.separator}$branch_name")
  }

  def get_all_branch_names(get_dir_files_from_string_function : (String) => Array[File] = File_Tools.get_dir_files_from_string,
                          ): List[String] = {
    val all_branchs_files = get_dir_files_from_string_function(s"${repo.get_sgith_path()}${File.separator}branch").toList
    all_branchs_files.map((file) => file.getName)
  }

  def get_branch_last_commit(branch_name : String,
                             read_in_file_function : (File) => List[String] = File_Tools.read_in_file):String = {
    val branch : File = get_branch_file(branch_name)
    val lines = read_in_file_function(branch)
    lines(0)
  }

  def change_last_commit_branch(sha1 : String, branch_name : String = get_current_branch_name(),
                                write_in_file_function : (File,String) => Unit = File_Tools.write_in_file,
                                branch_file : File = get_branch_file(get_current_branch_name())): Unit ={
    //Write into the head file the current branch name and its last commit
    branch_file.delete()
    val new_branch_file = new File(s"${repo.get_sgith_path()}${File.separator}branch${File.separator}$branch_name")
    new_branch_file.createNewFile()
    write_in_file_function(new_branch_file,sha1)
  }

  def get_current_commit_sha1(read_in_file_function : (File) => List[String] = File_Tools.read_in_file,
                              head_manager : Head_Manager= repo.get_head_manager()): String = {
    val HEAD_file = head_manager.get_HEAD_file()
    val parse : List[String] = read_in_file_function(HEAD_file)
    parse(0)
  }
}


object Branch_Manager {

  def main(branch_name: String): Unit = {
    //Add a file with the branch name in the branch repository
    Repository.get_repo_path() match {
      case Some(value) =>
        val repo: Repository = new Repository(value)
        val branch = Branch_Manager(repo)
        branch.create_branch_file(branch_name)
    }
  }

  def main_av(): Unit = {
    Repository.get_repo_path() match {
      case Some(value) =>
        val repo: Repository = new Repository(value)
        val branch = Branch_Manager(repo)
        val all_names = branch.get_all_branch_names()
        Output.print_sgit(all_names.mkString(" "))
    }
  }
}
