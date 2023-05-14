import java.io._
import scala.io.Source
import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException
import scala.util.Sorting
import scala.util.{Try, Success, Failure}

object localFile{
  val filename:String="C://Users//ionit//IdeaProjects//projectttt//src//main//scala//resources.txt"
  val value_filename:String="valuefile.txt"
}

object project extends App{

    def Task1_1():Unit= {
      val filename = localFile.filename
      val filedata = Source.fromFile(filename)
      try {
        val lines = filedata.getLines()
        for (line <- lines) {
          println(line)
        }
      }
      catch {
        case e: FileNotFoundException => {
          println("Error, file not found.")
        }
      }
      filedata.close()
    }

    def Task1_2(): Unit = {
      val filename = localFile.filename
      println("What renewable source would you like to remove/add?")
      val source = scala.io.StdIn.readLine()
      println("How many would you like to add/remove? Please add a minus if you want to remove (ex -2)")
      val number = scala.io.StdIn.readInt()
      val filedata = Source.fromFile(filename)
      try {
        val lines = filedata.getLines().toArray
        val writer = new PrintWriter(new File(filename))
        for (line <- lines) {
          val elements = line.split("\\s+")
          val first = elements.head
          if (source == first) {
            try {
              val lastelement = elements.last.toInt
              if (number < 0 && number.abs > lastelement) {
                println("You cannot remove this many.")
                writer.println(line)
              } else {
                val updatedlastelement = lastelement + number
                val updatedline = (elements.dropRight(1) :+ updatedlastelement.toString).mkString(" ")
                writer.println(updatedline)
              }
            } catch {
              case e: NumberFormatException =>
                println(s"Error: Cannot convert '${elements.last}' to an integer")
                writer.println(line)
            }
          } else {
            writer.println(line)
          }
        }
        writer.close()
        filedata.close()
      } catch {
        case e: FileNotFoundException =>
          println("Error, file not found.")
      }
    }

    def Task2():Unit= {
      val filename = localFile.filename
      val valuefile = new File(localFile.value_filename)
      val writer = new PrintWriter(valuefile)
      val filedata = Source.fromFile(filename)
      for (line <- filedata.getLines) {
        val elements = line.split("\\s+")
        val lastelement = elements.last.toInt
        val firstelement = elements.head
        var a = 0
        for (a <- 1 to lastelement) {
          println("How many watts per hour does the source produce?")
          try {
            val watts = scala.io.StdIn.readInt()
            println("How many hours per day does it work?")
            val hours = scala.io.StdIn.readInt()
            val daily = watts * hours
            val weekly = daily * 7
            val monthly = weekly * 4
            writer.println(s"$firstelement($a): $watts/hour $daily/day $weekly/week $monthly/month")
          }
          catch {
            case e: Exception =>
              println(s"Caught an exception: $e")
          }
        }
      }
      writer.close()
      filedata.close()
    }
    def Task3_1():Unit={
      val filename = localFile.value_filename
      try {
        val filedata = Source.fromFile(filename)
        for (line <- filedata.getLines) {
          println(line)
        }
        filedata.close()
      }
      catch {
        case e: FileNotFoundException => {
          println("Error, file not found.")
        }
      }

    }

    def Task3_2(): Unit = {
      val filename = localFile.value_filename
      println("What resource would you like to change? Format should be similar to 'Solar_panels(x)', where x is the number of the specific solar panel.")
      val source = scala.io.StdIn.readLine()
      try {
        val filedata=Source.fromFile(filename)
        val lines = filedata.getLines().toArray
        val writer = new PrintWriter(new File(filename))
        for (line <- lines) {
          val elements = line.split("\\s+")
          val first = elements.head
          val first2=first.substring(0,first.length-1)
          if (source == first2) {
            println("How many watts per hour does the source produce?")
            val watts = scala.io.StdIn.readInt()
            println("How many hours per day does it work?")
            val hours = scala.io.StdIn.readInt()
            val daily = watts * hours
            val weekly = daily * 7
            val monthly = weekly * 4
            writer.println(s"$first2: $watts/hour $daily/day $weekly/week $monthly/month")
          }
          else {
            writer.println(line)
          }
        }
        writer.close()
        filedata.close()
      } catch {
        case e: FileNotFoundException =>
          println("Error: file not found.")
      }
    }
    def Task5(): Unit = {
      val filename = localFile.value_filename
      try {
        val filedata = Source.fromFile(filename)
        val lines = filedata.getLines().toArray
        for (line <- lines) {
          val first= line.split("\\s+").head
          val first2=first.substring(0,first.length-1)
          val watts = line.split(":")(1).split("/")(0).replaceAll("[^\\d]", "").toInt
          val daily = line.split(":")(1).split("/")(1).replaceAll("[^\\d]", "").toInt
          if (watts < 250) {
            println(s"Low energy output. A source can not produce less than 250 watts per hour. Try changing the output of $first2.")
          }
          if (daily==0){
            println(s"Malfunction. The source $first2 is not powered.")
          }
        }
        filedata.close()
      } catch {
        case e: FileNotFoundException =>
          println("Error: file not found.")
      }
    }

    Task4.data_analysis(Task4.caselist) //Task4
    Task4.OrderAlgo(Task4.caselist) //ordering part of Task4- returns new caselist
    Task1_1()
    Task1_2()
    Task2()
    Task3_1()
    Task3_2()
    Task3_1()
    Task5()


  }

  object Task4 {

    case class energy_sources(name: String, hourly: Int, daily: Int, weekly: Int, monthly: Int)

    val filename1 = localFile.value_filename
    val filedata = Source.fromFile(filename1)
    val list = filedata.getLines().toList
    //println(list)

    val newlist = list.map(_.split(" ").map(_.trim).toList)
    val anotherlist = newlist.map(x => List(x(0).replace(":", ""), x(1).replace("/hour", ""), x(2).replace("/day", ""), x(3).replace("/week", ""), x(4).replace("/month", "")))

    //println(anotherlist)
    def recursiveCaseclassCreation(list: List[List[String]], caselist: List[energy_sources], index: Int, listlength: Int): List[energy_sources] = {
      val values = list(index)
      val energ = energy_sources(values(0), values(1).toInt, values(2).toInt, values(3).toInt, values(4).toInt)
      //name       hourly            daily            weekly            monthly
      //Int value error handling was done in the input part of the program, therefore no error handling should be required here when parsing int
      val newcaselist = caselist :+ energ
      if ((index + 1) < listlength) {
        val retcaselist = recursiveCaseclassCreation(list, newcaselist, index + 1, listlength)
        retcaselist
      }
      else newcaselist}

    val caselist = recursiveCaseclassCreation(anotherlist, List.empty[energy_sources], 0, anotherlist.length)

    //println(caselist)

    def order_hourly(caselist: List[energy_sources]): List[energy_sources] = {
      (caselist.sortWith(_.hourly < _.hourly)).foreach {
        x => println(s"${x.name}: ${x.hourly}/hour")
      }
      caselist.sortWith(_.hourly < _.hourly)
    }

    def order_daily(caselist: List[energy_sources]): List[energy_sources] = {
      (caselist.sortWith(_.daily < _.daily)).foreach {
        x => println(s"${x.name}: ${x.daily}/day")
      }
      caselist.sortWith(_.daily < _.daily)
    }

    def order_weekly(caselist: List[energy_sources]): List[energy_sources] = {
      (caselist.sortWith(_.weekly < _.weekly)).foreach {
        x => println(s"${x.name}: ${x.weekly}/week")
      }
      caselist.sortWith(_.weekly < _.weekly)
    }

    def order_monthly(caselist: List[energy_sources]): List[energy_sources] = {
      (caselist.sortWith(_.monthly < _.monthly)).foreach {
        x => println(s"${x.name}: ${x.monthly}/month")
      }
      caselist.sortWith(_.monthly < _.monthly)
    }

    def OrderHOF(caselist: List[energy_sources], argument: Int): List[energy_sources] = {
      def func_apply(caselist: List[energy_sources], f: List[energy_sources] => List[energy_sources]): List[energy_sources] = {
        f(caselist)
      }

      if (argument == 1) func_apply(caselist, order_hourly)
      else if (argument == 2) func_apply(caselist, order_daily)
      else if (argument == 3) func_apply(caselist, order_weekly)
      else if (argument == 4) func_apply(caselist, order_monthly)
      else {
        println("invalid choice. Try again")
        caselist
      }
    }

    def OrderAlgo(caselist: List[energy_sources]): List[energy_sources] = {
      def get_input(): Option[Int] = {
        println("Please choose a criteria by which the data should be sorted: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input. No changes were made to the data.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input. No changes were made to the data.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => OrderHOF(caselist, x)
        case None => caselist
      }
    }

    def extract_hourly_values(caselist: List[energy_sources]): List[Int] = {
      val newlist = caselist.map(x => x.hourly)
      newlist
    }

    def extract_daily_values(caselist: List[energy_sources]): List[Int] = {
      val newlist = caselist.map(x => x.daily)
      newlist
    }

    def extract_weekly_values(caselist: List[energy_sources]): List[Int] = {
      val newlist = caselist.map(x => x.weekly)
      newlist
    }

    def extract_monthly_values(caselist: List[energy_sources]): List[Int] = {
      val newlist = caselist.map(x => x.monthly)
      newlist
    }









    def MeanAlgo(caselist: List[energy_sources]): Unit = {
      def mean_HOF(caselist: List[energy_sources], f: List[energy_sources] => List[Int]): Double = {
        val newlist = f(caselist)
        val result: Double = (newlist.sum).toDouble / (newlist.length).toDouble
        result
      }
      def get_input(): Option[Int] = {
        println("Please choose the values from which you want the mean to be calculated: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => x match {
          case 1 => println(mean_HOF(caselist, extract_hourly_values))
          case 2 => println(mean_HOF(caselist, extract_daily_values))
          case 3 => println(mean_HOF(caselist, extract_weekly_values))
          case 4 => println(mean_HOF(caselist, extract_monthly_values))
        }
        case None => ()
      }
    }

    def MedianAlgo(caselist: List[energy_sources]): Unit = {
      def median_HOF(caselist: List[energy_sources], f: List[energy_sources] => List[Int]): Int = {
        val newlist = (f(caselist)).sorted
        if ((newlist.length) % 2 == 0) {
          val value1 = newlist((newlist.length / 2) - 1)
          val value2 = newlist((newlist.length) / 2)
          val result = (value1 + value2) / 2

          result
        }
        else {
          val result = newlist(((newlist.length) / 2))
          result
        }
      }
      def get_input(): Option[Int] = {
        println("Please choose the values from which you want the median to be calculated: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => x match {
          case 1 => println(median_HOF(caselist, extract_hourly_values))
          case 2 => println(median_HOF(caselist, extract_daily_values))
          case 3 => println(median_HOF(caselist, extract_weekly_values))
          case 4 => println(median_HOF(caselist, extract_monthly_values))
        }
        case None => ()
      }
    }

    def ModeAlgo(caselist: List[energy_sources]): Unit = {
      def mode_HOF(caselist: List[energy_sources], f: List[energy_sources] => List[Int]): Int = {
        val newlist = f(caselist)
        val newmap = newlist.groupBy(identity).mapValues(_.size)
        val (result, max) = (newmap.maxBy(_._2))
        result
      }


      def get_input(): Option[Int] = {
        println("Please choose the values from which you want the mode to be calculated: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => x match {
          case 1 => println(mode_HOF(caselist, extract_hourly_values))
          case 2 => println(mode_HOF(caselist, extract_daily_values))
          case 3 => println(mode_HOF(caselist, extract_weekly_values))
          case 4 => println(mode_HOF(caselist, extract_monthly_values))
        }
        case None => ()
      }
    }

    def RangeAlgo(caselist: List[energy_sources]): Unit = {
      def range_HOF(caselist: List[energy_sources], f: List[energy_sources] => List[Int]): Int = {
        val newlist = f(caselist)
        val max = newlist.max
        val min = newlist.min
        max - min
      }
      def get_input(): Option[Int] = {
        println("Please choose the values from which you want the range to be calculated: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => x match {
          case 1 => println(range_HOF(caselist, extract_hourly_values))
          case 2 => println(range_HOF(caselist, extract_daily_values))
          case 3 => println(range_HOF(caselist, extract_weekly_values))
          case 4 => println(range_HOF(caselist, extract_monthly_values))
        }
        case None => ()
      }
    }

    def MidrangeAlgo(caselist: List[energy_sources]): Unit = {
      def midrange_HOF(caselist: List[energy_sources], f: List[energy_sources] => List[Int]): Double = {
        val newlist = f(caselist)
        val max = newlist.max
        val min = newlist.min
        ((max + min).toDouble) / 2.0d
      }
      def get_input(): Option[Int] = {
        println("Please choose the values from which you want the midrange to be calculated: \n1 - Hourly\n2 - Daily\n3 - Weekly\n4 - Monthly\nChoose by inserting an integer from 1-4:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 5) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => x match {
          case 1 => println(midrange_HOF(caselist, extract_hourly_values))
          case 2 => println(midrange_HOF(caselist, extract_daily_values))
          case 3 => println(midrange_HOF(caselist, extract_weekly_values))
          case 4 => println(midrange_HOF(caselist, extract_monthly_values))
        }
        case None => ()
      }
    }

    def data_analysis_choice(caselist: List[energy_sources], argument: Int): Unit = {
      if (argument == 1) MeanAlgo(caselist)
      else if (argument == 2) MedianAlgo(caselist)
      else if (argument == 3) ModeAlgo(caselist)
      else if (argument == 4) RangeAlgo(caselist)
      else if (argument == 5) MidrangeAlgo(caselist)
      else println("Invalid choice")
    }

    def data_analysis(caselist: List[energy_sources]): Unit = {
      def get_input(): Option[Int] = {
        println("Please what sort of data analysis you'd like: \n1 - Mean\n2 - Median\n3 - Mode\n4 - Range\n5 - Midrange\nChoose by inserting an integer from 1-5:")
        val input = scala.io.StdIn.readLine()
        Try(input.toInt) match {
          case Success(x) => {
            if (x > 0 && x < 6) Some(x)
            else {
              println("Invalid input.")
              None
            }
          }
          case Failure(x) => {
            println("Invalid input.")
            None
          }
        }
      }

      get_input() match {
        case Some(x) => data_analysis_choice(caselist, x)
        case None => ()
      }

    }

  }