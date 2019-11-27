package Local_Changes

import java.io.File

import Create.Repository
import IO.File_Tools

class Index_Manager(repo : Repository) {

  def reset_index(new_index_lines : Option[List[String]] = None ,
                  write_file_function : (File,String)  => Unit = File_Tools.write_in_file,
                  delete_file_function : (File) => Unit = File_Tools.delete_File,
                  create_file_function : (String) => Boolean = File_Tools.create_file ,
                  get_local_file_function : (String) => File = repo.get_local_file,
                  sgit_path : String = repo.get_sgith_path()): Unit ={
    new_index_lines match {
      case Some(lines) => {
        get_local_file_function("index").delete()
        create_file_function(s"$sgit_path${File.separator}index")
        val new_index = get_local_file_function("index")
        if (lines.length != 0){
          write_file_function(new_index,lines.mkString("\n"))
        }
      }
      case None => get_local_file_function("index").delete()
        new File(s"$sgit_path${File.separator}index").createNewFile()
    }
  }

  def get_index_lines(get_local_file_function : (String) => File = repo.get_local_file,
                      read_in_file_function : (File) => List[String] = File_Tools.read_in_file) : List[String] = {
    val index : File = get_local_file_function("index")
    read_in_file_function(index)
  }


  def rebuild_index(blobs_lines : List[String], former_index : List[String],
                    new_index : File = repo.get_local_file("index"),
                    diff : Diff_Manager = repo.get_diff_manager(),
                    write_in_file_function : (File,String) => Unit = File_Tools.write_in_file,
                    get_working_tree_file_from_index_line_function : (String) => File = repo.get_working_tree_file_from_index_line): File = {
    //This function check which index file
    if (former_index.isEmpty || blobs_lines.isEmpty){
      if (blobs_lines.isEmpty && former_index.isEmpty){
        new_index
      }else
      if(blobs_lines.isEmpty) {
        val wt_file = get_working_tree_file_from_index_line_function(former_index.head : String)
        if(!wt_file.exists()){
          if(former_index.head.split(" ").last == "deleted"){
            write_in_file_function(new_index, former_index.head )
          }
          else{
            write_in_file_function(new_index, former_index.head + " deleted")
          }
        }else{write_in_file_function(new_index, former_index.head )}
        rebuild_index(blobs_lines,former_index.tail,new_index)
      }
      else{
        write_in_file_function(new_index,blobs_lines.head)
        rebuild_index(blobs_lines.tail,former_index,new_index)
      }
    }else {
      val relationship : Relationship = diff.check_index_changes(blobs_lines, former_index.head)
      relationship.change_type match {
        case "modified" | "unmodified" |"renamed" | "created" =>
          write_in_file_function(new_index,relationship.index_line)
          val new_blobs_lines = blobs_lines.filter( _ != relationship.index_line)
          rebuild_index(new_blobs_lines, former_index.tail, new_index)

        //If there is no new_blob it means it has been deleted
        case "deleted" => {
          val wt_file = get_working_tree_file_from_index_line_function(former_index.head: String)
          if (!wt_file.exists()) {
            //If the file has been deleted we check if the file has already been marqued deleted
            if (former_index.head.split(" ").last == "deleted") {
              write_in_file_function(new_index, former_index.head)
            } else {
              write_in_file_function(new_index, former_index.head + " deleted")
            }
          } else {
            write_in_file_function(new_index, former_index.head)
          }
          val new_blobs = blobs_lines
          rebuild_index(new_blobs, former_index.tail, new_index)
        }
      }
    }
  }
}
