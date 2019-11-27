package Local_Changes

import java.io.File
import Create.Repository
import IO.File_Tools

import scala.io.{BufferedSource, Source}

case class Blob(file: File){
  val source: BufferedSource = scala.io.Source.fromFile(file.getAbsolutePath)
  val content: Iterator[String] = try source.getLines finally source.close()
  val content_string: String = File_Tools.read_in_file(file).mkString("")
  val content_list_string: List[String] = File_Tools.read_in_file(file)
  val name: String = file.getName
  val sha1: String = File_Tools.calulate_sha1(content_string + name)
  val working_tree_file_path : String = file.getAbsolutePath
  val dir_name : String = sha1.slice(0,2)
  val file_name : String = sha1.slice(2,sha1.length)
}

class Blob_Manager(repo : Repository){

  def blobs_create(files: List[File] ): List[Blob] = {
    //This function create blobs from the given files it stores them into the .sgit/blob
    @scala.annotation.tailrec
    def loop(files: List[File],  blob_list: List[Blob] = List[Blob](),
             sgit_path : String = repo.get_sgith_path(),
             blob_file_exist_function : (Blob,String) => Boolean = blob_file_exists,
             create_blob_function : (Blob, (File,String) => Unit,String) => File = create_blob_file,
             write_in_file_function : (File,String) => Unit = File_Tools.write_in_file): List[Blob] ={
      if (files.isEmpty){
        return blob_list
      }
      val file = files.head
      val blob_file = Blob(file)
      if (!blob_file_exist_function(blob_file,sgit_path)){
        create_blob_file(blob_file,write_in_file_function)
      }
      loop( files.tail ,blob_list.appended(blob_file))
    }
    loop(files)

  }


  def create_blob_index_line(blob : Blob): String = {
    s"${blob.dir_name}${blob.file_name} ${blob.working_tree_file_path}"
  }

  def get_blob_list_index_line(blobs : List[Blob],
                               create_blob_index_line_function : (Blob) => String = create_blob_index_line): List[String] = {
    blobs.map((blob) => create_blob_index_line_function(blob))
  }

  def blob_file_exists(blob : Blob, sgit_path : String = repo.get_sgith_path()): Boolean = {
    val real_path = s"$sgit_path${File.separator}blob${File.separator}${blob.dir_name}${File.separator}${blob.file_name}"
    val blob_file =new File(real_path)
    blob_file.exists()
  }

  def create_blob_file(blob: Blob, write_file_function : (File,String)  => Unit = File_Tools.write_in_file,
                       sgit_path : String = repo.get_sgith_path()): File = {
    //We do a callback in order to keep the function as a first citizen
    val real_path_dir = s"$sgit_path${File.separator}blob${File.separator}${blob.dir_name}"
    val real_path_file = s"$sgit_path${File.separator}blob${File.separator}${blob.dir_name}${File.separator}${blob.file_name}"
    new File(real_path_dir).mkdir()
    val blob_file = new File(real_path_file)
    blob_file.createNewFile()
    write_file_function(blob_file, blob.working_tree_file_path + "\n" + blob.content_list_string.mkString("\n"))
    blob_file
  }

  def get_blob_file_from_index_line(index_line : String,
                                    get_object_file_from_sha1_function : (String,String) => File = repo.get_object_file_from_sha1): File = {
    val parse_1 = Repository.parse_index_line(index_line)
    val sha1_1 = parse_1(0)
    get_object_file_from_sha1_function(sha1_1,"blob")
  }
}

object Blob_Manager {



def get_blob_working_tree_file_path(file : File,
                                    read_in_file_function : (File) => List[String] = File_Tools.read_in_file): String ={
  val lines = read_in_file_function(file)
  val l = lines.slice(0,1)
  l.mkString("")
}

  def get_blob_file_content(file : File,
                            read_in_file_function : (File) => List[String] = File_Tools.read_in_file): List[String] = {
    val lines = read_in_file_function(file)
    val l = lines.slice(1,lines.length)
    l
  }

}
