import scala.io.Source

import scala.io.Source
import scala.annotation.tailrec

object Detect {
  //Case class: Thresholds and Indexes, are defined to represent thresholds and data column indexes, respectively.

  // Use Option for thresholds to make it more flexible and safer
  case class lower_value(wind: Option[(Double, Double)], hydro: Option[(Double, Double)])
  // Use Option for index to make it safer
  case class position(wind: Option[Int], hydro: Option[Int], status: Option[Int])

  // Here we use tail recursive to handle lines
  //This way, the compiler will optimize it to avoid stack overflow during recursion.
  @tailrec
  def Lines_dealing(lines: List[String], positions: position, start: lower_value, outlier: List[(String, String)] = List.empty): List[(String, String)] = {
    //This function handles data rows read from a file.
    // Pattern matching was used to handle different situations of row list lines.

    lines match {
      //In the case of an empty list (Nil), the function returns the anomalies parameter, which is a list of detected exceptions.
      case Nil => outlier

      //When the list is not empty, the function first checks whether the current line is empty.
      // If it is empty, skip the row and recursively call handleLines to process the next row.
      case line :: tail if line.nonEmpty =>
        val items = line.split(",")
        //Detect if the wind speed exceeds the preset threshold and handle errors in the program
        val wind_Anomaly = positions.wind.flatMap(index => items.lift(index).flatMap(item =>
          try {
            val windPower = item.toDouble
            start.wind.filter(t => windPower < t._1 || windPower > t._2).map(_ =>
              s"Wind power anomaly detected outlier: $windPower")
          } catch {
            case _: NumberFormatException => None
          }
        ))

        //Same treatment for hydroelectric power generation
        val hydro_outlier = positions.hydro.flatMap(index => items.lift(index).flatMap(item =>
          try {
            val hydroPower = item.toDouble
            start.hydro.filter(t => hydroPower < t._1 || hydroPower > t._2).map(_ => s"Hydro power anomaly detected outlier: $hydroPower")
          } catch {
            case _: NumberFormatException => None
          }
        ))
          //Here is solar and detect the status
        val equipment_outlier = positions.status.flatMap(index => items.lift(index).flatMap(item =>
          try {
            val equipment_Status = item.toInt
            if (equipment_Status == 0)
              Some("Equipment malfunction detected outlier :Solar panel disconnect") else
              None
          } catch {
            case _: NumberFormatException => None
          }
        ))

        val newAnomalies = List(wind_Anomaly, hydro_outlier, equipment_outlier).flatten.map((line, _))
        Lines_dealing(tail, positions, start, outlier ++ newAnomalies)
      case _ :: tail =>
        Lines_dealing(tail, positions, start, outlier)
    }
  }

  //It performs anomaly detection on the data in the file.
  def detect_outlier(filename: String): Unit = {
    val start = lower_value(Some((50.0, 3000.0)), Some((100.0, 20000.0)))
    val positions = position(Some(6), Some(7), Some(11))

    val lines = Source.fromFile(filename).getLines().toList
    val Lines = if (lines.headOption.contains("y"))
      lines.tail else
      lines

    //Call the handleLines function to detect anomalies in the data and save the results in the aliases variable.
    val anomalie = Lines_dealing(Lines, positions, start)

    //if an exception is detected, print out the exception information and the number of rows where the exception is located;
    // Otherwise, print 'No anomalies detected.'
    if (anomalie.nonEmpty) {
      println("\nAnomalies detected:")
      anomalie.foreach {
        case (line, anomaly) => println(s"$anomaly\nLine: $line\n")
      }
    } else {
      println("No anomalies detected.")
    }
  }
}
