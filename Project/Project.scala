import java.time.LocalDate
import scala.io.{Source, StdIn}
import scala.util.Try

object Project {
  val solar = Solar()
  val wind = Wind()
  val hydro = Hydro()

  //Read data from the specified file. If the file cannot be read using an error message
  def readData(fileName: String): List[String] = {
    try {
      Source.fromFile(fileName).getLines().toList
    } catch {
      case e: Exception =>
        println(s"Error reading file: ${e.getMessage}")
        List.empty[String]
    }
  }

  //Provide a method to read console input and attempt to parse the input into an integer type.
  // If successful, returns the integer value of Some, otherwise returns None.
  def Read_Option(str: String): Option[Int] = {
    println(str)
    Try(StdIn.readInt()).toOption
  }

    //The difference is that we read the files and then put them into an array to return.
    // And this is also part of the HOF
  def readData1(fileName: String): Array[String] = {
    val source = Source.fromFile(fileName)
    val lines = try {
      source.getLines().toArray
    } finally {
      source.close()
    }
    lines
  }

  //Write the modified content into the file. The code uses the higher-order function mkString. Also reported an error
  def writeData(fileName: String, lines: Array[String]): Unit = {
    val writer = new java.io.PrintWriter(new java.io.File(fileName))
    writer.write(lines.mkString("\n"))
    writer.close()
  }

  //Main function
  def mainMenu(): Unit = {
    var option = true

    //print menu
    while (option) {
      println()
      println("Please select an option:")
      println("1. Monitor and Control")
      println("2. Store Data")
      println("3. Show view and Adjust")
      println("4. Analysis data")
      println("5. Detect and Alert")
      println("0. Exit")

      //Option types and match statements were used to handle possible null value situations and different option situations.
      // And if the user selects a number other than 0-5, an error will be reported
      Read_Option("Enter your option:") match {

        //Option 1 is when the user wants to monitor the system and view the current energy situation.
        // And it can be changed by some values as needed
        case Some(1) =>

          // We have a csv file: Energy.csv that consists of 17 lines: y, m, d, Time, Time zone 019; solor power, wind power, hydroelectric power, Pressure (msl) (hPa),Precipitation amount (mm), temperature (degC), Number of solar panels, Solar panel angle, Turbine speed (rev/min), windmill wind direction, water level
          val data = Source.fromFile("energy.csv").getLines().toList

          //We use HOF here:monitor_status and get monitor status
          solar.monitor_status(data, status => {
            println("")
          })
          wind.monitor_status(data, status => {
            println("Wind status: " + status)
          })
          hydro.monitor_status(data, status => {
            println("Hydro status: " + status)
          })

          //This is let user input his choice to adjust this system
          println("Do you want to adjust? (Y/N)")
          StdIn.readLine().toUpperCase match {
            case "Y" =>
              println("Select adjustment option:")
              println("1. Adjust Number of solar panels")
              println("2. Adjust Windmill Wind Direction")
              println("3. Adjust Turbine Speed")

              //And Actual modification processing here. And we use HOF: processAdjustment
              StdIn.readInt() match {
                case 1 => solar.process_Adjust("energy.csv", "Number of solar panels",writeData ,readData1)
                case 2 => wind.process_Adjust("energy.csv", "windmill wind direction",writeData ,readData1)
                case 3 => hydro.process_Adjust("energy.csv", "Turbine speed (rev/min)",writeData ,readData1)
                case _ => println("Invalid option.")
              }
              println("Successfully adjust.")
            case "N" => // Do nothing

            //Error information
            case _ => println("Invalid input. Please enter Y or N.")
          }

        //The second option is to place the data of solar, wind, and hydroelectric power generation in different CSV files.
        // Convenient for users to read data
        case Some(2) =>
          //Load data
          val data = Source.fromFile("energy.csv").getLines().toList
          solar.store_Data(data, "solar.csv")
          println("Solar power data have successfully stored in solar.csv")
          wind.store_Data(data, "wind.csv")
          println("Wind power data have successfully stored in wind.csv")
          hydro.store_Data(data, "hydro.csv")
          println("Hydro power data have successfully stored in hydro.csv")

          /*
          Here we create a view.
          And we allow users to modify some data, which will be modified in previously generated separate files and energy files, such as Solar Panel Angle, Windmill Wind Direction and Turbine Speed.
          This way, users can control the increase or decrease of energy
           */
        case Some(3) =>
          solar.Analysis_EnergyData("solar.csv")
          wind.Analysis_EnergyData("wind.csv")
          hydro.Analysis_EnergyData("hydro.csv")
          println("Do you want to adjust? (Y/N)")
          StdIn.readLine().toUpperCase match {
            case "Y" =>
              println("Select adjustment option:")
              println("1. Adjust Solar Panel Angle")
              println("2. Adjust Windmill Wind Direction")
              println("3. Adjust Turbine Speed")
              StdIn.readInt() match {
                case 1 => Data_Adjust.process_Adjust("solar.csv", "Solar panel angle",writeData ,readData1)
                case 2 => Data_Adjust.process_Adjust("wind.csv", "windmill wind direction",writeData ,readData1)
                case 3 => Data_Adjust.process_Adjust("hydro.csv", "Turbine speed (rev/min)",writeData ,readData1)
                case _ => println("Invalid option.")
              }
              println("Successfully adjust.")
            case "N" => // Do nothing
            case _ => println("Invalid input. Please enter Y or N.")
          }

          /*
          Option 4 allows users to analyze data. Users can choose to analyze one hour, one day, one month, one year, and custom data.
          When we select a day, the system will calculate the average, median, mode, range, and median values for the day selected by the user.
          And it will print out three different energy data separately
           */
        case Some(4) =>
          Analysis_data.analysis_Data()

          /*
          Option 5 is to detect and alert. We have set the minimum and maximum thresholds.
          When it is found that the data does not have any values or exceeds the limit, an alarm will be issued.
          And print all the information where the alarm is located
           */
        case Some(5) =>
          Detect.detect_outlier("energy.csv")

          //Stop the program
        case Some(0) =>
          option = false

          //Error information
        case _ =>
          println("Invalid option. Please enter a number between 0 and 5.")
      }
    }
  }

  //Run our program
  def main(args: Array[String]): Unit = {
    mainMenu()
  }

}
