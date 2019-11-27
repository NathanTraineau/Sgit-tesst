package Create

import java.io.File

import Branches.Branch_Manager
import IO.{File_Tools, Output}
import Local_Changes.{Blob_Manager, Commit_Manager, Diff_Manager, Head_Manager, Index_Manager}


class Repository(repo_path : String) {
  //Everything in this class is made with absolute path of files

  val diff_manager = new Diff_Manager(this)
  val sgit_path = s"$repo_path${File.separator}.sgit"
  val branch_manager : Branch_Manager = new Branch_Manager(this)
  val blob_manager : Blob_Manager = new Blob_Manager(this)
  val commit_manager : Commit_Manager = new Commit_Manager(this)
  val index_manager : Index_Manager = new Index_Manager(this)
  val head_manager : Head_Manager = new Head_Manager(this)


  def get_sgith_path(): String ={
    sgit_path
  }

  def get_repo_path(): String ={
    repo_path
  }

  def get_diff_manager() : Diff_Manager = {
    diff_manager
  }

  def get_index_manager() : Index_Manager = {
    index_manager
  }

  def get_branch_manager(): Branch_Manager ={
    branch_manager
  }

  def get_blob_manager(): Blob_Manager ={
    blob_manager
  }

  def get_commit_manager(): Commit_Manager ={
    commit_manager
  }

  def get_head_manager(): Head_Manager = {
    head_manager
  }

  def get_local_file(local_path : String): File = {
    //This function return the local file, which pathname has been given in parameter
    new File(s"${get_sgith_path()}${File.separator}$local_path")
  }

  def get_object_file_from_sha1(sha1 : String, object_type : String):File = {
    val dir = sha1.slice(0,2)
    val filename = sha1.slice(2,sha1.length)
    get_local_file((s"$object_type${File.separator}$dir${File.separator}$filename"))
  }

  def get_working_tree_file_from_index_line(index_line : String): File = {
    //This function take a line of the index with the format sha1_name_Option["deleted"] ( _ for the space )
    val blob_file : File = get_object_file_from_sha1(Repository.parse_index_line(index_line)(0),"blob")
    new File(Blob_Manager.get_blob_working_tree_file_path(blob_file) : String)
  }

  def initialize_current_branch(branch : Branch_Manager = get_branch_manager()): Unit = {
    branch.create_branch_file("master","0"*40)
    branch.change_current_branch("master")
  }


}


object Repository {

  def initialize_repo(path : String, create_dir_function : (String) => Boolean = File_Tools.create_dir,
                      create_file_function : (String) => Boolean = File_Tools.create_file
                     ): Unit = {
    // Input : path = the current directory path of the user or the path of a child directory
    //println(pathRepo)
    //This function create a new repository, it means that it create a .sgit directory with
    //.sgit/objects/ : the object store, which we’ll introduce in the next section.
    //.sgit/refs/ the reference store. It contains two subdirectories, heads and tags.
    //.sgit/HEAD, a reference to the current HEAD (a ref to the current branch we are working on)
    //.sgit/logs the directory that keep tracks of everything that happend : it has a Head file and a subdirectory
    //refs where we store every commit of every
    val sgit_path: String = s"$path${File.separator}.sgit"
    if (!File_Tools.path_exists(path)) {
      create_dir_function(path)
    }
    if (repo_exists()) {
      Output.print_already_repo()
    }
    else {
      create_dir_function(s"$path${File.separator}.sgit")
      val folders_name = List( "branch", "blob", "commit");
      folders_name.map((folder) => create_dir_function(s"$sgit_path${File.separator}$folder"))
      val files_name = List("HEAD", "index");
      files_name.map((file) => create_file_function(s"$sgit_path${File.separator}$file"))
    }
//We intitalize the files (we insert in HEAD and master branch 40 * "0"
    get_repo_path(path) match {
      case Some(value) =>
        val repo: Repository = new Repository(value)
        repo.initialize_current_branch()
      case None =>
    }
  }

  //////////////////// Files Managing //////////////////

  def repo_exists(): Boolean ={
    get_repo_path().nonEmpty
  }

  def is_Repo(dir_path : String, repo_dir : String = ".sgit"): Boolean = {
    File_Tools.is_dir_in(dir_path, repo_dir)
  }

  @scala.annotation.tailrec
  def get_repo_path(current_path: String = System.getProperty("user.dir") , repo_dir : String = ".sgit"): Option[String] = {
    // This function return, if it exists, the path of the repo of the current file
    // As an .sgit directory must be in its parent we climb the tree until the
    // root dir
    //It returns the path and not the file because we need the path for further operation
    //for example to create other file which function take a path as parameter.
    if (!File_Tools.path_exists(current_path)) None

    val current_dir = new File(current_path)
    if (is_Repo(current_dir.getAbsolutePath,repo_dir))
      Some(s"${current_dir.getAbsolutePath}")
    else {
      if (current_dir.getParentFile == null) None // We are at the root directory
      else get_repo_path(current_dir.getParentFile.getAbsolutePath, repo_dir)
    }
  }

  def parse_index_line(index_line : String): List[String] = {
    val sha1 = index_line.split(" ")(0)
    val name = index_line.split(" ")(1)
    List(sha1,name)
  }



}
