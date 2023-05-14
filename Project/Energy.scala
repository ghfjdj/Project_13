import Project.{readData1, writeData}
import java.io.PrintWriter
import java.time.LocalDate

/*
This code defines three case classes: Solar, Wind, and Hydro, all of which inherit a trait named energyPlant.
This trait defines some common methods, such as monitor_status, storeData, printFinalEnergyData, and processAdjustment.
 */
//We used type parameters and covariant type parameter +T.
trait energyPlant[+T <: energyPlant[T]] {
  def monitor_status(data: List[String], processFunc: String => Unit): Unit
  def store_Data(data: List[String], fileName: String): Unit
  def Analysis_EnergyData(file: String): Unit
  def process_Adjust(fileName: String, energyType: String,writeData: (String,Array[String])=>Unit,readData:String=>Array[String]): Unit
}

case class Solar() extends energyPlant[Solar] {

  //monitor_status: displays the latest status of each energy power station, including number of panels, angle, Hourly power generation.
  override def monitor_status(data: List[String], processFunc: String => Unit): Unit = {
    val lastLine = data.last.split(",")
    println(
      s"""\n********Solar power********
         |${lastLine(11)} solar panels are running currently,
         |panel angle: ${lastLine(12)}，
         |Hourly power generation: ${lastLine(5)}""".stripMargin)
  }

  //StoreData: Writes the data of each energy power station into a file in a specified format.
  override def store_Data(data: List[String], fileName: String): Unit = {
    val solarData = data.map(line => {
      val position1 = line.split(",")
      List(position1(0), position1(1), position1(2), position1(5), position1(11), position1(12)).mkString(",")
    })
    new PrintWriter(fileName) {
      write(solarData.mkString("\n"));
      close
    }
  }

  //Analysis the final energy data and specific indicators in a specific format,
  // such as average, median, mode, range, and median values
  override def Analysis_EnergyData(file: String): Unit = {
    Analysis_data.EnergyData(file, "Solar Energy", "Solar Panel Angle")
  }

  //Data adjustment
  override def process_Adjust(fileName: String, energyType: String,writeData: (String,Array[String])=>Unit,readData:String=>Array[String]): Unit = {
    Data_Adjust.process_Adjust(fileName, energyType,writeData,readData1)
  }
}

//
case class Wind() extends energyPlant[Wind] {

  ////monitor_status: displays the latest status of each energy power station, including Windmill direction, Current wind speed, Hourly power generation.
  override def monitor_status(data: List[String], processFunc: String => Unit): Unit = {
    val lastLine = data.last.split(",")
    println(
      s"""\n********Wind power********
         |Windmill direction: ${lastLine(14)}，
         |Current wind speed: ${lastLine(16)}，
         |Hourly power generation: ${lastLine(6)}""".stripMargin)
  }

  //Choose the specific line of the data
  override def store_Data(data: List[String], fileName: String): Unit = {
    val windData = data.map(line => {
      val items = line.split(",")
      List(items(0), items(1), items(2), items(6), items(14), items(16)).mkString(",")
    })
    new PrintWriter(fileName) {
      write(windData.mkString("\n"));
      close
    }
  }

  //Analysis the data of Wind
  override def Analysis_EnergyData(file: String): Unit = {
    Analysis_data.EnergyData(file, "Wind Energy", "Windmill Wind Direction")
  }

  //Higher order function
  override def process_Adjust(fileName: String, energyType: String,writeData: (String,Array[String])=>Unit,readData:String=>Array[String]): Unit = {
    Data_Adjust.process_Adjust(fileName, energyType,writeData ,readData)
  }
}

case class Hydro() extends energyPlant[Hydro] {
  ////monitor_status: displays the latest status of each energy power station, including Current turbine speed, current water level, Hourly power generation.
  override def monitor_status(data: List[String], processFunc: String => Unit): Unit = {
    val lastLine = data.last.split(",")
    println(
      s"""\n********Hydro power********
         |Current turbine speed: ${lastLine(13)}，
         |The current water level: ${lastLine(15)}，
         |Hourly power generation: ${lastLine(7)}""".stripMargin)
  }

  //Choose the specific line of the data
  override def store_Data(data: List[String], fileName: String): Unit = {
    val hydroData = data.map(line => {
      val items = line.split(",")
      List(items(0), items(1), items(2), items(7), items(13), items(15)).mkString(",")
    })
    new PrintWriter(fileName) {
      write(hydroData.mkString("\n"));
      close
    }
  }

  override def Analysis_EnergyData(file: String): Unit = {
    Analysis_data.EnergyData(file, "Hydro Energy", "Turbine Speed")
  }

  //Higher order function here
  override def process_Adjust(fileName: String, energyType: String,writeData: (String,Array[String])=>Unit,readData:String=>Array[String]): Unit = {
    Data_Adjust.process_Adjust(fileName, energyType,writeData ,readData)

  }
}



