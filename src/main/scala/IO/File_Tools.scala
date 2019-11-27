package IO

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.security.MessageDigest

import scala.io.Source

object File_Tools {

  def create_dir(path : String): Boolean = {
    val new_dir = new File(path).mkdir()
    new_dir
  }

  def create_file(path : String): Boolean = {
    val new_file = new File(path).createNewFile()
    new_file
  }

  /////////////////// Find entities ////////////////////

  def path_exists(path: String): Boolean = {
    //Function which takes a path and a name as parameter
    //Return true if there is the file in the directory, false if not
    new File(path).exists()
  }

  def is_dir_in(current_dir: String =".", dir: String): Boolean = {
      new File(s"${current_dir}${File.separatorChar}$dir").isDirectory
  }


  def is_file_in(current_dir: File = new File("."), file: String): Boolean = {
    new File(s"${current_dir.getAbsolutePath}${File.separatorChar}$file").isFile
  }

  def get_dirs_files_from_string(path_dirs : List[String]): List[File] ={
    val op = path_dirs.flatMap((path_dir) => get_dir_files_from_string(path_dir ))
    op
  }

  def get_dir_files_from_string(path_dir :String): Array[File] = {
    val f = get_dir_files(new File(path_dir))
    f
  }

  def get_dir_files(dir: File): Array[File] = {
    // This function get every files from the given File (file or directory)
    // and the file from the child of it
    if (!dir.exists()) {
      Output.print_dir_not_exists(dir.getName)
      return Array[File]()
    }
    if (dir.isFile){
      Array(dir)
    }
    else
      dir.listFiles().filter(_.getName != ".sgit").flatMap(get_dir_files _)
  }


  def delete_File(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(delete_File(_))
    }
    file.delete()
  }


/////////////////// Operation on Files /////////////////////

  def calulate_sha1(string : String): String ={
    //This function takes a string and return the sha1 affiliated
    val md = MessageDigest.getInstance("SHA-1")
    md.digest(string.getBytes("UTF-8")).map("%02x".format(_)).mkString
  }

  def write_in_file(file : File, text : String): Unit ={
    val bw = new BufferedWriter(new FileWriter(file,true))
    bw.write(text + "\n")
    bw.close()
  }

  def read_in_file(file : File): List[String] ={
    val filename = file.getAbsolutePath
    val bufferedSource = Source.fromFile(filename)
    val lines = bufferedSource.getLines()
    val l = lines.toList
    bufferedSource.close
    l//garezgzg
  }






}
