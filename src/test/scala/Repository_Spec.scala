import IO.File_Tools
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}
import java.io.{File, PrintWriter}

import Create.Repository
import Local_Changes.{Blob, Blob_Manager}

class Repository_Spec extends FlatSpec with Matchers with BeforeAndAfter  {


  before{
    val user_dir: String = System.getProperty("user.dir")
    val sgit = new File(s"$user_dir${File.separator}.sgit")
    if (sgit.exists()) {
      delete(sgit)
      sgit.delete()
    }

    val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
    val test2 = new File(s"$user_dir${File.separator}test2")
    if (test2.exists()) {
      delete(test2)
    }
    val test4 = new File(s"$user_dir${File.separator}test4")
    if (test4.exists()) {
      delete(test4)
    }
    val index = new File(s"$user_dir${File.separator}index")
    if (index.exists()) {
      delete(index)
    }
  }

  after {
    val user_dir: String = System.getProperty("user.dir")
    val sgit = new File(s"$user_dir${File.separator}.sgit")
    if (sgit.exists()) {
      delete(sgit)
      sgit.delete()
    }

    val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
    val test2 = new File(s"$user_dir${File.separator}test2")
    if (test2.exists()) {
      delete(test2)
    }
    val test4 = new File(s"$user_dir${File.separator}test4")
    if (test4.exists()) {
      delete(test4)
    }
    val index = new File(s"$user_dir${File.separator}index")
    if (index.exists()) {
      delete(index)
    }
  }

  def delete(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(delete(_))
    }
    file.delete()
  }

  "The function intialize_repo" should "create the files structure" in {
    val user_dir: String = System.getProperty("user.dir")
    Repository.initialize_repo(user_dir)
    val sgit_dir_path = s"$user_dir${File.separator}.sgit"
    assert(new File(sgit_dir_path).exists())
    assert(new File(s"$sgit_dir_path${File.separator}HEAD").exists())
    assert(new File(s"$sgit_dir_path${File.separator}index").exists())
    assert(new File(s"$sgit_dir_path${File.separator}branch").exists())
    assert(new File(s"$sgit_dir_path${File.separator}blob").exists())
    assert(new File(s".sgit${File.separator}commit").exists())
  }

  "The function repo_exists" should "return True if the repo exists, false otherwise" in {
    val user_dir: String = System.getProperty("user.dir")
    print("azerzaerzar   " + user_dir)
    assert(!Repository.repo_exists())
    Repository.initialize_repo(user_dir)
    assert(Repository.repo_exists())
  }

  "The function get_repo_path" should "return the repo_path from a pathname, none if there is no" in {
    val user_dir: String = System.getProperty("user.dir")
    assert(Repository.get_repo_path().isEmpty)
    File_Tools.create_dir(".sgit")
    Repository.get_repo_path() match {
      case Some(value) => assert(value == user_dir,"The function found the wrong repository")
      case _ => fail("The function didn't find the repository")}
  }


////////////////// Test on the Repository Class /////////////////////////////////////

  "create_blob_file" should "create the good file at the right place" in {
      val user_dir: String = System.getProperty("user.dir")
      Repository.initialize_repo(user_dir)

    val file = new File(s"$user_dir${File.separator}test4")
    if (file.exists()){
      file.delete()
    }
    file.createNewFile()
      val str = "This is a test"
      File_Tools.write_in_file(file, str )
      val blobMock = Blob(file)
      val repo :Repository = new Repository(user_dir)
      val real_path = s"${repo.get_sgith_path()}${File.separator}blob${File.separator}${blobMock.dir_name}${File.separator}${blobMock.file_name}"
      val blob_file =new File(real_path)
      assert(!blob_file.exists())
      repo.get_blob_manager().create_blob_file(blobMock,File_Tools.write_in_file)
      assert(blob_file.exists())
      assert(blob_file.getAbsolutePath === real_path)
      assert(blob_file.getName === blobMock.file_name )
      assert(blob_file.getParentFile.getName === blobMock.dir_name )
  }


}
