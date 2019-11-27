import java.io.File

import IO.File_Tools
import Local_Changes.{Change, Diff_Manager}
import org.scalatest.{BeforeAndAfter, FlatSpec, Matchers}

class Diff_Manager_Spec extends FlatSpec with Matchers with BeforeAndAfter {

  before {
    val user_dir: String = System.getProperty("user.dir")
    val sgit = new File(s"$user_dir${File.separator}.sgit")
    if (sgit.exists()) {
      delete(sgit)
    }
   val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
    val test2 = new File(s"$user_dir${File.separator}test2")
    if (test2.exists()) {
      delete(test2)
    }
    val test5 = new File(s"$user_dir${File.separator}test5")
    if (test5.exists()) {
      delete(test5)
    }
  }

  after {
    val user_dir: String = System.getProperty("user.dir")
    val sgit = new File(s"$user_dir${File.separator}.sgit")
    if (sgit.exists()) {
      delete(sgit)
    }
    val test = new File(s"$user_dir${File.separator}test")
    if (test.exists()) {
      delete(test)
    }
    val test2 = new File(s"$user_dir${File.separator}test2")
    if (test2.exists()) {
      delete(test2)
    }
    val test5 = new File(s"$user_dir${File.separator}test5")
    if (test5.exists()) {
      delete(test5)
    }

  }

    def delete(file: File): Unit = {
      if (file.isDirectory) {
        file.listFiles().foreach(delete(_))
      }
      file.delete()
    }


    "get_modif_files" should "return the smallest amount of Modification Object between 2 files" in {
      val user_dir: String = System.getProperty("user.dir")
      val file1 = new File(s"$user_dir${File.separator}test5")
      if (file1.exists()){
        delete(file1)
        file1.delete()
      }
      file1.createNewFile()
      val str = "Hello Tout le monde !\nCeci est un test\n Ca ca y est depuis le debut\n Ca c'est modiff"
      File_Tools.write_in_file(file1, str )

      val file2 = new File(s"$user_dir${File.separator}test2")
      if (file2.exists()){
        delete(file2)
      }
      file2.createNewFile()
      val str2 = "Ceci est un test\n Bonjour je viens d'ajouter ca\n Et ca aussi\n Etttttttttttttt ca alors ?\n Oui oui ca aussi\n Ca ca y est depuis le debut\n Ca c'est modif"
      File_Tools.write_in_file(file2, str2 )

      val changes = Diff_Manager.get_modif_files(File_Tools.read_in_file(file2),File_Tools.read_in_file(file1))
      assert(Diff_Manager.get_modif_files(File_Tools.read_in_file(file2),File_Tools.read_in_file(file1)) === List(Change("-",0,"Hello Tout le monde !" ), Change("",0,"Ceci est un test"), Change("+",1, " Bonjour je viens d'ajouter ca" ), Change("+",2, " Et ca aussi" ), Change("+",3, " Etttttttttttttt ca alors ?" ), Change("+",4, " Oui oui ca aussi"), Change("",5, " Ca ca y est depuis le debut"), Change("+",6, " Ca c'est modif"), Change("-",7, " Ca c'est modiff")))
    }


}
