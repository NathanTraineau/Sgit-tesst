package IO

object Output {

  def print_dir_not_exists(dir_path : String = ""): Unit ={
    println(s"$dir_path This directory doesn't exists")
  }

  def print_sgit(str : String): Unit = {
    print(str)
  }

  def print_already_repo():Unit = {
    print_sgit("There already is a repository")
  }

}
