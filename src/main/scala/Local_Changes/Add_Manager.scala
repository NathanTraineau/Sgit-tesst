package Local_Changes

import java.io.File

import Create.Repository
import IO.File_Tools

object Add_Manager {

  def main(input: List[String]): Unit = {
    //We check if we are in an existing repository & Get the path
    Repository.get_repo_path() match {
      case Some(value) =>
        val repo: Repository = new Repository(value)
        val blob_manager = repo.get_blob_manager()
        val index_manager = repo.get_index_manager()
        val head_manager = repo.get_head_manager()
        val branch = repo.get_branch_manager()
        //We get the files that the user wants to add
        var added_files: List[File] = List[File]()
        if (input.head == ".") {
          added_files = File_Tools.get_dirs_files_from_string(System.getProperty("user.dir") :: Nil) : List[File]
        }
        else {
          added_files = File_Tools.get_dirs_files_from_string(input): List[File]
        }
        //We create the affiliated blobs into the sgit blob directory
        val blob_list = blob_manager.blobs_create(added_files)
        val blob_index_lines = blob_manager.get_blob_list_index_line(blob_list)
        //We create a new index with these blobs considering the former index
        val former_index : List[String] = index_manager.get_index_lines()
        index_manager.reset_index()
        val new_index = index_manager.rebuild_index(blob_index_lines,former_index)
      case _ => println("No repository found")
    }
  }


}
