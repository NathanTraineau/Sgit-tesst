package Local_Changes

import java.io.File

import Create.Repository
import IO.{File_Tools, Output}




case class Change( change_type : String , line : Int, changed_string : String )
//Change_type is either deleted (-) or added (+)
case class Relationship(new_blob_file : Option[File], former_Blob_File : Option[File],
                        change_type : String, index_line : String){
  val new_Blob_file_name: String = new_blob_file match {
    case Some(file) => file.getName
    case None => ""
  }
  val former_Blob_File_name: String = former_Blob_File match {
    case Some(file) => file.getName
    case None => ""
  }
  val former_blob_File_content: List[String] = former_Blob_File match {
    case Some(file) => Blob_Manager.get_blob_file_content(file)
    case None =>  List[String]()
  }
  val new_blob_file_content: List[String] = new_blob_file match {
    case Some(file) => Blob_Manager.get_blob_file_content(file)
    case None => List[String]()
  }
  val list_changes: List[Change] = Diff_Manager.get_modif_files(new_blob_file_content,former_blob_File_content)
}

case class Set_Relationship( unmodified : List[Relationship] = List[Relationship](),
                             modified : List[Relationship] = List[Relationship](),
                             renamed : List[Relationship] = List[Relationship](),
                             created : List[Relationship] = List[Relationship](),
                             deleted : List[Relationship]= List[Relationship]())

class Diff_Manager(repo : Repository) {

  def check_files_status(blobs_lines : List[String],
                         index_lines : List[String],
                         unmodified : List[Relationship] = List[Relationship](),
                         modified : List[Relationship] = List[Relationship](),
                         renamed : List[Relationship] = List[Relationship](),
                         created : List[Relationship] = List[Relationship](),
                         deleted : List[Relationship]= List[Relationship](),
                         check_index_changes_function : (List[String], String) => Relationship = check_index_changes,
                         get_blob_file_from_index_line_function : (String,(String,String)=> File) => File = repo.get_blob_manager().get_blob_file_from_index_line): Set_Relationship = {
    //This function check which index file

    if (index_lines.isEmpty || blobs_lines.isEmpty ){
      if (blobs_lines.isEmpty && index_lines.isEmpty){
        return Set_Relationship(unmodified,modified,renamed,created,deleted)
      }
      if(blobs_lines.isEmpty) {
        val index_line_file = get_blob_file_from_index_line_function(index_lines.head,repo.get_object_file_from_sha1)
        if(index_lines.head.split(" ").last != "deleted"){
          //If the file has already been detected as deleted we consider the delete as "added"
          val rel : Relationship = Relationship(None,Some(index_line_file),"deleted",index_lines.head)
          check_files_status(blobs_lines, index_lines.tail, unmodified, modified, renamed, created, deleted.appended(rel))
        }else{
          check_files_status(blobs_lines, index_lines.tail, unmodified, modified, renamed, created, deleted)
        }
        //List of the deleted files

      }else{
        val blob_file = get_blob_file_from_index_line_function(blobs_lines.head,repo.get_object_file_from_sha1)
        val rel : Relationship = Relationship(Some(blob_file),None,"created",blobs_lines.head)
        //The Created files
        //add_blob_to_index(blobs.head, new_index)
        check_files_status(blobs_lines.tail,index_lines,unmodified,modified,renamed,created.appended(rel),deleted)
      }
    }else {
      val rel : Relationship = check_index_changes_function(blobs_lines, index_lines.head)
      rel.change_type match {
        case "unmodified" =>
          //add_blob_to_index(blob, new_index)
          val new_blobs = blobs_lines.filter( _ != rel.index_line)
          check_files_status(new_blobs,index_lines.tail,unmodified.appended(rel),modified,renamed,created,deleted)
        case "modified" =>
          //add_blob_to_index(blob, new_index)
          val new_blobs = blobs_lines.filter( _ != rel.index_line)
          check_files_status(new_blobs,index_lines.tail,unmodified,modified.appended(rel),renamed,created,deleted)
        case "renamed" =>
          //add_blob_to_index(blob, new_index)
          val new_blobs = blobs_lines.filter( _ != rel.index_line)
          check_files_status(new_blobs,index_lines.tail,unmodified,modified,renamed.appended(rel),created,deleted)
        case _ =>
          if(index_lines.head.split(" ").last != "deleted"){
            //If the file has already been detected as deleted we consider the delete as "added"
            val new_blobs = blobs_lines
            check_files_status(new_blobs,index_lines.tail,unmodified,modified,renamed,created,deleted.appended(rel))
          }else{
            check_files_status(blobs_lines, index_lines.tail, unmodified, modified, renamed, created, deleted)
          }
          }
      }
    }

  def check_index_changes(new_index_lines : List[String], index_line : String ): Relationship  = {
    //This function send back the status of the given blob considering the given index
    //It goes through all of the file and detect similarities or not ( created/deleted)
    @scala.annotation.tailrec
    def loop(new_index_lines : List[String], index_line : String  ): Relationship ={
      val parse_2 = Repository.parse_index_line(index_line)
      val sha1_2 = parse_2(0)
      val name_2 = parse_2(1)
      val index_line_file = repo.get_object_file_from_sha1(sha1_2,"blob")
      if (new_index_lines.isEmpty) {
        Relationship(None,Some(index_line_file),"deleted","")
      }
      else {
        val parse_1 = Repository.parse_index_line(new_index_lines.head)
        val sha1_1 = parse_1(0)
        val name_1 = parse_1(1)
        val file_new_file_line = repo.get_object_file_from_sha1(sha1_1,"blob")

        if (sha1_1 == sha1_2 && name_1 == name_2){
          Relationship(Some(file_new_file_line),Some(index_line_file),"unmodified",new_index_lines.head)
        }
        else if (sha1_1 == sha1_2) {
          Relationship(Some(file_new_file_line),Some(index_line_file),"renamed",new_index_lines.head)
        }
        else if (name_1 == name_2) {
          Relationship(Some(file_new_file_line),Some(index_line_file),"modified",new_index_lines.head)
        } else
        {
          loop(new_index_lines.tail, index_line)
        }
      }
    }
    loop(new_index_lines,index_line)
  }


  def get_set_relationship(blob_Manager: Blob_Manager = repo.get_blob_manager(),
                           index_Manager: Index_Manager = repo.get_index_manager()): Set_Relationship = {
        //We get the files that the user wants to add
        val working_tree_files = File_Tools.get_dirs_files_from_string(s"${repo.get_repo_path()}" :: Nil): List[File]
        val blob_list = blob_Manager.blobs_create(working_tree_files)
        val index = index_Manager.get_index_lines()
        val blob_list_lines = blob_Manager.get_blob_list_index_line(blob_list)
        val diff_class = new Diff_Manager(repo)
        diff_class.check_files_status(blob_list_lines, index)
    }
  def get_set_relationship_static(newLines : List[String],
                                  formerLines:List[String]): Set_Relationship = {
    check_files_status(newLines, formerLines)
  }
  }
  object Diff_Manager {

    def main(): Unit = {
      Repository.get_repo_path() match {
        case Some(value) => {
          val repo: Repository = new Repository(value)
          val diff_class = new Diff_Manager(repo)
          val set_Relationship : Set_Relationship = diff_class.get_set_relationship()
          val diff_string : String = diff_toString(set_Relationship)
          Output.print_sgit(diff_string)
          }
          case _ => None
        }
      }


    def get_modif_files(new_file_lines :List[String], former_file_lines :List[String]): List[Change] = {
      //This function compare the 2 given files and gives the changes between these 2
      // It should return the smallest amount of Modification Object possible between these 2 files
      
if(new_file_lines.size == 1 && new_file_lines.head == ""){
	return List[Change]()
}
def loop(new_file_lines :List[String], former_file_lines :List[String], list_changes: List[Change] = List[Change](), line : Int = 0 ): List[Change] = {
      if(new_file_lines.isEmpty && former_file_lines.isEmpty){
        //We are at the end and we would like to take only the smallest liste of changes
        list_changes
      }else {

        if (new_file_lines.isEmpty) {
          val change2 = new Change("-", line, former_file_lines.head)
          return loop(new_file_lines, former_file_lines.tail, list_changes.appended(change2), line)
        }
        if (former_file_lines.isEmpty) {
          val change1 = new Change("+", line, new_file_lines.head)
          return loop(new_file_lines.tail, former_file_lines, list_changes.appended(change1), line + 1)
        }

        if (new_file_lines.head == former_file_lines.head) {
          val no_change = new Change("", line, new_file_lines.head)
          loop(new_file_lines.tail, former_file_lines.tail, list_changes.appended(no_change), line + 1)
        }
        else {
          //We observe both scenarios :
          //The line were removed from the former file
          val change1 = Change("+", line, new_file_lines.head)
          val add = loop(new_file_lines.tail, former_file_lines, list_changes.appended(change1), line + 1)
          //The line were created into the new file
          val change2 = Change("-", line, former_file_lines.head)
          val supp = loop(new_file_lines, former_file_lines.tail, list_changes.appended(change2), line)
          if (add.size > supp.size) {
            supp
          } else {
            add
          }
        }
      }
    }
    loop(new_file_lines,former_file_lines)
  }



  def diff_toString(set_Relationship: Set_Relationship, str : String ="",
                    get_blob_working_tree_file_path_function : (File,(File) => List[String]) => String = Blob_Manager.get_blob_working_tree_file_path,
                    read_in_file_function : (File) => List[String] = File_Tools.read_in_file): String={
//if modified. pas empty :
// et apreès on passe au suivant created...
    if(set_Relationship.modified.nonEmpty) {
      val relationship = set_Relationship.modified.head
      val name = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been modified \n"
      val modif_file = relationship.list_changes
      val modif_to_string = local_diff_toString(modif_file)
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified.tail,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted)
      diff_toString(new_set, str + modified_introduction + modif_to_string + "\n")
    }else
    if(set_Relationship.deleted.nonEmpty) {
      val relationship = set_Relationship.deleted.head
      val name = relationship.former_Blob_File match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val modified_introduction = s"${name} has been deleted \n"
      val modif_file = relationship.list_changes
      val modif_to_string = local_diff_toString(modif_file)
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed,set_Relationship.created,set_Relationship.deleted.tail)
      diff_toString(new_set, str + modified_introduction + modif_to_string + "\n")
    }else
    if(set_Relationship.renamed.nonEmpty) {
      val relationship = set_Relationship.renamed.head
      val name_new_blob = relationship.new_blob_file match {
        case Some(file) => get_blob_working_tree_file_path_function(file,read_in_file_function)
        case None => ""
      }
      val name_former_blob = relationship.former_Blob_File match {
        case Some(file) => relationship.index_line.split(" ")(1)
        case None => ""
      }
      val new_set = Set_Relationship(set_Relationship.unmodified,set_Relationship.modified,set_Relationship.renamed.tail,set_Relationship.created,set_Relationship.deleted)
      diff_toString(new_set, str +  name_former_blob  + " has been renamed into => "  + name_new_blob +  "\n")

    }else {
      //We return the string
       str
    }
  }



  def local_diff_toString(changes : List[Change], str : String =""): String = {
    //Print the list of changes given in parameter
    // It should look like this
    //--- a/li
    //+++ b/li
    //@@ -1,4 +1,7 @@
    //-Hello tout le monde
    // ceci est un test
    //+Bonjour je viens d'ajouter ca
    //+et ca aussi
    //+et ca aussi
    //+oui oui ca aussi
    // ca ca y est depuis le debut
    //-ca c'est modiff
    //+ca c'est modif
    if(changes.isEmpty){
      return str
    }
    val count_type_occ :List[Int] = count_type(changes)
    if(str.length == 0){
      local_diff_toString(changes, str + s"${count_type_occ(0)} inserted (+)  ${count_type_occ(1)} deleted (-) \n")
    }else {
      val change = changes.head
      change.change_type match {
        case "-" => local_diff_toString(changes.tail, str + s"${Console.RED} ${change.change_type}  ${change.changed_string} ${Console.RESET}" + "\n")
        case "+" => local_diff_toString(changes.tail, str + s"${Console.GREEN} ${change.change_type}  ${change.changed_string} ${Console.RESET}" + "\n")
        case _ => local_diff_toString(changes.tail, str + s"${Console.WHITE} ${change.change_type}  ${change.changed_string} ${Console.RESET}" + "\n")
      }
    }
  }


  @scala.annotation.tailrec
  def count_type(changes : List[Change], created : Int = 0, deleted : Int = 0): List[Int] = {
    if(changes.isEmpty){
      return List(created,deleted)
    }
    changes.head.change_type match {
      case "-" => count_type(changes.tail,created,deleted +1)
      case "+" => count_type(changes.tail,created +1,deleted )
      case _ => count_type(changes.tail,created ,deleted )
    }
  }
}
