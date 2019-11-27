package Local_Changes

import java.io.File

import Create.Repository
import IO.File_Tools

class Head_Manager(repo : Repository) {

  def update_HEAD(sha1 : String,
                  write_in_file_function : (File,String) => Unit = File_Tools.write_in_file,
                  sgit_path : String = repo.get_sgith_path()): Unit ={
    //Write into the head file the current branch name and its last commit
    val head = get_HEAD_file()
    head.delete()
    val new_current = new File(s"$sgit_path${File.separator}HEAD")
    new_current.createNewFile()
    write_in_file_function(new_current,sha1)
  }

  def get_HEAD_file( get_local_file_function : (String) => File = repo.get_local_file ): File = {
    get_local_file_function("HEAD")
  }



}
