package IO

import java.text.SimpleDateFormat
import java.util.Date

object Date_Tool {

  def get_Date_As_String(d: Date = new Date): String = {
    val dateFormat = new SimpleDateFormat("EEE, MMM dd, yyyy h:mm a")
    dateFormat.format(d)
  }

}
