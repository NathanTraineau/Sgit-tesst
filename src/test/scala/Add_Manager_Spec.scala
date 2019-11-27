import java.io.{File, PrintWriter}

import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class Add_Manager_Spec extends FlatSpec with Matchers with BeforeAndAfter  {
  before {
    val user_dir: String = System.getProperty("user.dir")
    val testing_folder = new File(s"$user_dir${File.separator}testing_folder")
    if (testing_folder.exists()) {
      delete(testing_folder)
    }
  }

  after {
    val user_dir: String = System.getProperty("user.dir")
    val testing_folder = new File(s"$user_dir${File.separator}testing_folder")
    if (testing_folder.exists()) {
      delete(testing_folder)
    }
  }

  def delete(file: File): Unit = {
    if (file.isDirectory) {
      file.listFiles().foreach(delete(_))
    }
    file.delete()
  }
/*
Not working code,
  "main(input : List[String])" should "add the input given in the index" in {
    val user_dir: String = System.getProperty("user.dir")
    Repository.initialize_repo(s"$user_dir${File.separator}testing_folder")
    val repo: Repository = new Repository(s"$user_dir${File.separator}testing_folder")
    File_Tools.create_dir(s"$user_dir${File.separator}testing_folder")
    val folders_name = List("logs", "branch", "blob", "tree", "tags", "commit");
    folders_name.map((folder) => File_Tools.create_dir(s"$user_dir${File.separator}testing_folder${File.separator}$folder"))
    val files_name = List("HEAD", "index");
    files_name.map((file) => File_Tools.create_file(s"$user_dir${File.separator}testing_folder${File.separator}$file"))
    val files_name2 = List("HEADS", "refs");
    files_name2.map((file) => File_Tools.create_file(s"$user_dir${File.separator}testing_folder${File.separator}logs${File.separator}$file"))

    // We fulfill the files to make it more realistic
    files_name.map((filename) => {
      val file = new File(s"$user_dir${File.separator}testing_folder${File.separator}$filename")
      val pw = new PrintWriter(file)
      val str = "Ceci est un test sur " + file.getName
      pw.write(str)
      pw.close()
    })
    files_name2.map((filename) => {
      val file = new File(s"$user_dir${File.separator}testing_folder${File.separator}logs${File.separator}$filename")
      val pw = new PrintWriter(file)
      val str = "Ceci est un test sur logs " + file.getName
      pw.write(str)
      pw.close()
    })

    //Situation 1
    //In this situation, we should add each of the 4 files created
    Add.main(s"$user_dir${File.separator}testing_folder" :: Nil)
    val blob_mock = mock[List[Blob]]
    val repo_mock = mock[Repository]
    //repo_mock.rebuild_index(blob_mock) was called
    print("diiiiiiiiiiiiiiiiiiirrr" + File_Tools.get_dir_files_from_string(s"${System.getProperty("user.dir")}${File.separator}testing_folder") )
    //assert(repo.get_index_lines().size === 4)
    //assert(File_Tools.get_dir_files_from_string(s"testing_folder${File.separator}blob").length === 4)

    //Situation 2
    //In this situation, we should only add HEAD and index as the HEADS's path file is not given properly
    repo.reset_index()
    Add.main(List(s"testing_folder${File.separator}HEAD", s"testing_folder${File.separator}HEADS", s"testing_folder${File.separator}index"))

    //assert(repo.get_index_lines().size === 2)
    //assert(File_Tools.get_dir_files_from_string(s"testing_folder${File.separator}blob").length === 4)

    //Situation 3
    //In this situation we should add HEAD, HEADS and refs
    Add.main(List(s"testing_folder${File.separator}HEAD", s"testing_folder${File.separator}branch", s"testing_folder${File.separator}logs"))

    //assert(repo.get_index_lines().size === 4)
    //assert(File_Tools.get_dir_files_from_string(s"$user_dir${File.separator}testing_folder${File.separator}blob").length === 4)

    //Situation 4
    //We change the content of a file without resetting the index

    val file = new File(s"$user_dir${File.separator}testing_folder${File.separator}logs${File.separator}HEADS")
    val pw = new PrintWriter(file)
    val str = "Ceci est un test  " + file.getName
    pw.write(str)
    pw.close()

    Add.main(List(s"testing_folder${File.separator}HEAD", s"testing_folder${File.separator}branch", s"testing_folder${File.separator}logs"))

    //assert(repo.get_index_lines().size === 4)
    //assert(File_Tools.get_dir_files_from_string(s"$user_dir${File.separator}testing_folder${File.separator}blob").length === 5)

    //Situation 5
    //We ask for every

  }
*/
}
