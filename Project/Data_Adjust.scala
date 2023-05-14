import java.time.LocalDate
import scala.io.{Source, StdIn}

object Data_Adjust {

  //Used to handle adjustments to a certain type of energy. WriteData and readData are parameters passed in as a function
  def process_Adjust(fileName: String, energyType: String,writeData: (String,Array[String])=>Unit,readData:String=>Array[String]): Unit = {

    println(s"Enter new value for $energyType:")
    val newValue = StdIn.readLine().toDouble

    //Read data here
    val lines = readData(fileName)
    val line2 = readData("energy.csv")

    // Get the index of the column to adjust
    val headers = lines(0).split(",")
    val columnIndex = headers.indexOf(energyType)
    val adjusted_Lines = adjust_Data(lines, columnIndex, newValue, 1)

    val header2 = line2(0).split(",")
    val columnIndex2 = header2.indexOf(energyType)
    val adjustedLine2 = adjust_Data(line2, columnIndex2, newValue, 1)

    // Write the adjusted data back to the file
    writeData(fileName, adjusted_Lines)
    writeData("energy.csv", adjustedLine2)
  }

  //Function is a recursive function.
  // Its function is to adjust the value of a specific column in the lines array to be equal to newVal.
  // If the date is later than the target date, replace the value of the target column with newVal.
  def adjust_Data(lines: Array[String], column_posi: Int, newValue: Double, position: Int): Array[String] = {
    if (position >= lines.length) {
      return lines
    }

    val newline = lines(position).split(",")
    val date = LocalDate.of(newline(0).toInt, newline(1).toInt, newline(2).toInt)
    val final_Date = LocalDate.of(2018, 12, 1)

    // If date is later than targetDate, it sets the element at the columnIndex index position in the fields array to newVal
    if (date.isAfter(final_Date)) {
      newline(column_posi) = newValue.toString
      lines(position) = newline.mkString(",")
    }

    adjust_Data(lines, column_posi, newValue, position + 1)
  }
}
