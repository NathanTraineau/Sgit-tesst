import java.io.{File, PrintWriter}

import IO.File_Tools
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class File_Tools_Spec extends FlatSpec with Matchers with BeforeAndAfter{

  before{
    val user_dir: String = System.getProperty("user.dir")
    val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
val test6 = new File(s"$user_dir${File.separator}test6")
    if (test6.exists()) {
      delete(test6)
    }
val test3 = new File(s"$user_dir${File.separator}test3")
    if (test3.exists()) {
      delete(test3)
    }
    val dir1 = new File(s"$user_dir${File.separator}dir1")
    if (dir1.exists()) {
      delete(dir1)
    }
    val dir2 = new File(s"$user_dir${File.separator}dir2")
    if (dir2.exists()) {
      delete(dir2)
    }

    val file1 = new File(s"$user_dir${File.separator}file1")
    if (file1.exists()) {
      delete(file1)
    }
  }

  after {
    val user_dir: String = System.getProperty("user.dir")
    val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
val test6 = new File(s"$user_dir${File.separator}test6")
    if (test6.exists()) {
      delete(test6)
    }
val test3 = new File(s"$user_dir${File.separator}test3")
    if (test3.exists()) {
      delete(test3)
    }
    val dir1 = new File(s"$user_dir${File.separator}dir1")
    if (dir1.exists()) {
      delete(dir1)
    }
    val dir2 = new File(s"$user_dir${File.separator}dir2")
    if (dir2.exists()) {
      delete(dir2)
    }

    val file1 = new File(s"$user_dir${File.separator}file1")
    if (file1.exists()) {
      delete(file1)
    }
  }

  def delete(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(delete(_))
    }
    file.delete()
  }

  def create_dirs(dirs: List[String]): Unit={
    dirs.map((d) => File_Tools.create_dir(d))
  }

  def create_files(files: List[String]): Unit={
    files.map((file) => File_Tools.create_file(file))
  }

  "create_dir" should "create a directory in the given directory" in {
    val user_dir: String = System.getProperty("user.dir")
    val file = new File(s"$user_dir${File.separator}test5")
    if (file.exists()){
      delete(file)
    }
    //assert(!new File(s"$user_dir${File.separator}test").exists())
    File_Tools.create_dir(s"$user_dir${File.separator}test5")
    assert(new File(s"$user_dir${File.separator}test5").exists())
    //assert(new File(s"$user_dir${File.separator}test5").isDirectory)
  }

  "create_file" should "create a file in the given directory" in {
    assert(!new File("test").exists())
    File_Tools.create_file("test")
    assert(new File("test").exists())
    assert(new File("test").isFile)
  }

  "dir_in " should "return True when the searched directory is child of the given directory" in {
    val user_dir: String = System.getProperty("user.dir")
    val file = new File(s"$user_dir${File.separator}test6")
    if (file.exists()){
      delete(file)
    }
    assert(!File_Tools.is_dir_in(user_dir,s"test6"))
    File_Tools.create_dir(s"$user_dir${File.separator}test6")
    assert(File_Tools.is_dir_in(user_dir,s"test6"))
  }

  "get_dirs_files_from_string, get_dir_files and get_dir_files_from_string" should "give every files in the given directory" in {
    File_Tools.create_file("file1")
    File_Tools.create_dir("dir1")
    File_Tools.create_dir("dir2")
    File_Tools.create_file(s"dir2${File.separator}file4")
    File_Tools.create_file(s"dir1${File.separator}file3")
    File_Tools.create_file(s"dir1${File.separator}file2")
    assert(File_Tools.get_dirs_files_from_string("dir1 dir2".split(" ").toList).size === 3 )
    assert(File_Tools.get_dir_files_from_string("dir1") === File_Tools.get_dir_files(new File("dir1")) )
  }

  "write_in_file" should "write every given lines into the given file, not overwrite it" in {
    val user_dir: String = System.getProperty("user.dir")
    val file = new File(s"$user_dir${File.separator}test3")
    if (file.exists()){
      file.delete()
    }
    file.createNewFile()
    val str = "Bonjour Comment allez vous Moi bien"
    File_Tools.write_in_file(file,str)
    val content = File_Tools.read_in_file(file)
    assert(content(0) === str)
    val str2 = "Bonjour Comment allez vous Moi bieeeeeen"
    File_Tools.write_in_file(file,str2)
    val content2 = File_Tools.read_in_file(file)
    assert(content(0) != str2)
    assert(content2(0) === str)
    assert(content2(1) === str2)
  }

  "read_in_file" should "return a List[String] with every string in the file given in parameter" in {
    val user_dir: String = System.getProperty("user.dir")
    val file = new File(s"$user_dir${File.separator}test")
    file.createNewFile()
    val pw = new PrintWriter(file)
    val str = "Bonjour \nComment allez vous\n Moi bien"
    pw.write(str)
    pw.close()
    val content = File_Tools.read_in_file(file)
    assert(content.toArray === str.split("\n"))
  }

  "calculate sha1"  should "take the content of a file and return its right sha1" in {
    val str = "Hi this is a sha1 test"
    val str2 = "Hello how are you ?&é&(çà-&8r* $ $*'^t²)(hi)"
    assert(File_Tools.calulate_sha1(str).length == 40)
    assert(File_Tools.calulate_sha1(str2).length == 40)
    assert(File_Tools.calulate_sha1(str) != File_Tools.calulate_sha1(str2))
  }

}
