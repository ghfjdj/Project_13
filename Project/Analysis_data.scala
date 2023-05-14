import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.io.{Source, StdIn}

object Analysis_data {
  //Parse date string and using LocalDate
  def deal_Date(year: String, month: String, day: String): LocalDate =
    LocalDate.of(year.toInt, month.toInt, day.toInt)

  // Check if the date is before December 1, 2018
  def strict_time(date: LocalDate): Boolean =
    date.isBefore(LocalDate.of(2018, 12, 1))

  //Set initial value
  val Energy_Capacity = 50000000

  //Filter out data rows earlier than December 1, 2018, calculate the total energy output and the final equipment parameter values,
  //and finally print out the corresponding information
  def EnergyData(fileName: String, energyName: String, deviceName: String): Unit = {
    //Create a view here.
    val lines = Source.fromFile(fileName).getLines().drop(1).toList.view
    var total_Energy = 0.0
    var final_Device_value = ""

    lines.filter(line => {
      val lines = line.split(",")
      val date = deal_Date(lines(0), lines(1), lines(2))
      strict_time(date)
    }).foreach(line => {
      val lines = line.split(",")
      val energyProduced = lines(3).toDouble
      val deviceParam = lines(4)
      total_Energy += energyProduced
      final_Device_value = deviceParam
    })

    //Calculate the percentage of total energy generation to energy capacity
    val energyPercentage = total_Energy / Energy_Capacity * 100
    println(s"$energyName: Total Produced: $total_Energy, Capacity: $Energy_Capacity, Percentage: $energyPercentage%, Final $deviceName: $final_Device_value")
  }

  def analysis_Data(): Unit = {
    //print option let user to choose
    println("Enter the analysis option (1 for hourly, 2 for daily, 3 for weekly, 4 for monthly, 5 for custom range):")

    val analysis_Option = StdIn.readInt()
    val lines = Source.fromFile("energy.csv").getLines().drop(1).toList.view
    val Date_P = DateTimeFormatter.ofPattern("yyyy-M-d-H:mm")


    def DateTime(year: String, month: String, day: String, time: String): LocalDateTime =
      LocalDateTime.parse(s"$year-$month-$day-$time", Date_P)

    //This function uses recursion to process data for each energy type
    def process_Data(lines: List[String], position: Int = 5): Unit = {
      if (position > 7)
        return

      //Use higher-order function to process every row in the data
      val data = lines.map(line => line.split(",")).map(lines =>
        lines(position).toDouble
      )

      val Data_sorted = data.sorted

      //Calculate the data
      val min_value = Data_sorted.min
      val max_value = Data_sorted.max
      val sum = Data_sorted.sum
      val avg = sum / Data_sorted.length
      val median = if (Data_sorted.length % 2 == 0) {
        val midIndex = Data_sorted.length / 2
        (Data_sorted(midIndex - 1) + Data_sorted(midIndex)) / 2.0
      } else {
        Data_sorted(Data_sorted.length / 2)
      }

      //Calculate the most frequently occurring elements in a dataset. Use some higher-order function
      val mode = Data_sorted.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
      val range = max_value - min_value
      val midRange = (min_value + max_value) / 2

      if (position == 5)
        println(s"Solar power: ")
      if (position == 6)
        println(s"Wind power: ")
      if (position == 7)
        println(s"Hydropower: ")

      println(s"Average: $avg")
      println(s"Median: $median")
      println(s"Mode: $mode")
      println(s"Range: $range")
      println(s"Mid Range: $midRange")
      println()

      process_Data(lines, position + 1)
    }

    analysis_Option match {

      //case 1 for the analysis the hour data
      case 1 =>
        println("Enter the starting date and time (format: yyyy-MM-dd-HH:mm):")
        val startDateTime = StdIn.readLine()
        val startDate = LocalDateTime.parse(startDateTime, Date_P)
        val lines_Process = lines.filter(line => {
          val lines = line.split(",")
          val date = DateTime(lines(0), lines(1), lines(2), lines(3))
          date.isAfter(startDate.minusHours(1)) && date.isBefore(startDate.plusHours(1))
        }).toList
        process_Data(lines_Process)

        //case 2 for the analysis the daily data
      case 2 =>
        println("Enter the date (format: yyyy-MM-dd):")
        val Date = StdIn.readLine()
        val date = LocalDateTime.parse(Date + "-00:00", Date_P)
        val lines_Process = lines.filter(line => {
          val value = line.split(",")
          val datetime = DateTime(value(0), value(1), value(2), value(3))
          datetime.toLocalDate.isEqual(date.toLocalDate)
        }).toList
        process_Data(lines_Process)

        //case 3 for the analysis the weekly data
      case 3 =>
        println("Enter the date (format: yyyy-MM-dd):")
        val Date = StdIn.readLine()
        val date = LocalDateTime.parse(Date + "-00:00", Date_P)
        val lines_Process = lines.filter(line => {
          val value = line.split(",")
          val datetime = DateTime(value(0), value(1), value(2), value(3))
          datetime.toLocalDate.isAfter(date.toLocalDate.minusDays(7)) && datetime.toLocalDate.isBefore(date.toLocalDate.plusDays(1))

        }).toList
        process_Data(lines_Process)

        //case 4 for the analysis the monthly data
      case 4 =>
        println("Enter the date (format: yyyy-MM):")
        val Date = StdIn.readLine()
        val date = LocalDateTime.parse(Date + "-01-00:00", Date_P)
        val lines_Process = lines.filter(line => {
          val value = line.split(",")
          val datetime = DateTime(value(0), value(1), value(2), value(3))
          datetime.toLocalDate.isAfter(date.toLocalDate) && datetime.toLocalDate.isBefore(date.toLocalDate.plusMonths(1))
        }).toList
        process_Data(lines_Process)

        //Users can select data from any date
      case 5 =>

        //input the start data and deal with it
        println("Enter the starting date (format: yyyy-MM-dd-HH:mm):")
        val startDateString = StdIn.readLine()
        val startDate = LocalDateTime.parse(startDateString, Date_P)
        //input the end data and deal with it
        println("Enter the ending date (format: yyyy-MM-dd-HH:mm):")
        val endDate = StdIn.readLine()
        val end = LocalDateTime.parse(endDate, Date_P)

        val lines_Process = lines.filter(line => {
          val value = line.split(",")
          val datetime = DateTime( value(0),  value(1),  value(2),  value(3))
          datetime.isAfter(startDate.minusHours(1)) && datetime.isBefore(end.plusHours(1))
        }).toList
        process_Data(lines_Process)

        //Error information
      case _ => println("Invalid option.")
    }
  }

}
