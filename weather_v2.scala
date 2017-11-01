/*
by Arjuna Das @31st Oct 2017
changes:
	--ISO datae format
	-- lat/Long range calculation
	-- Result to file , ask for filename and check if exists
*/
import scala.util.Random
import java.sql.Timestamp
import java.text.SimpleDateFormat

//import java.io._  //java io class for file write (output file)


class Weather{

var city = Array("Delhi","Mumbai","Chennai","Sydney","Melbourne","Hyderabad","Lahore"
			,"Singapore","Kuala Lumpur","Bangalore","Canberra","Adelaide"
			,"Kolkata","Istanbul","London","Bangkok" )

//later addaed to controlling decimal places
def truncTo(n: Double, p: Int): Double 
  = { val s = math pow (10, p); (math floor n * s) / s }

// Longitude & Longitude random generation . Assuming range between -x to +x
def getLongLati(x:Int):Double={
    var num = (x * -1) + (Math.random()*(x - (x * -1))+1);
    //var pos = Math.floor(Math.random());
    //if (pos == 0) {
      //  num = num * -1;
    //}
    truncTo(num,2);   //truncating to 2 decimal places
}

// to get randome datetime. java JDBC timestamp used
def getDateTime: String= {

  val st_dt = Timestamp.valueOf("2016-01-01 00:00:00").getTime();
  val en_dt = Timestamp.valueOf("2017-01-01 00:00:00").getTime();
  val diff = en_dt - st_dt + 1;
  val random = new Random(System.nanoTime) // You may want a different seed
  val new_d=new Timestamp(st_dt + (random.nextInt(diff.toInt)));
  new SimpleDateFormat("YYYY-MM-dd hh:mm:ss").format(new_d);
}

/// as per https://en.wikipedia.org/wiki/Barometric_formula

//Random elevation between 11 to 200
def getElve:Int={
  val r = scala.util.Random
  r.nextInt(100)
}

//to get temp based on elev 
def getTemp(elev1:Int): Double={
    val elev = elev1.toDouble/1000
    if (elev <= 11)
       288.15 - (6.5 * elev) - 273.15
    else if (elev <= 20)
      	216.65
    else if (elev <= 32)
       196.65 + elev - 273.15
    else if (elev <= 47)
      228.65 + 2.8 * (elev - 32) - 273.15
    else if (elev <= 51)
      270.65 - 273.15
    else if (elev <= 71)
      270.65 - 2.8 * (elev - 51) - 273.15
    else if (elev <= 84.85)
      214.65 - 2 * (elev - 71) - 273.15
    else
      0
}

//Function to geopotential attribute to calculate the pressure of location from web
  def get_geopotential(elev:Int):Double={
    val EARTH_RADIUS = 6356.766
    EARTH_RADIUS * elev / (EARTH_RADIUS + elev)
}

//to get pressure based on elev and temp
def getPress(elev:Int):Double ={
         val geopot_elev = get_geopotential(elev)
    val t = getTemp(geopot_elev.toInt) + 273.15 //Converting to Kelving for formula compatibility

 if (geopot_elev <= 11)
	(101325 * math.pow(288.15 / t, -5.255877)/100)  //Dividing by 100 to convert from Pasals to hecto pascals
   else if (geopot_elev <= 20)
	(22632.06 * math.exp(-0.1577 * (geopot_elev - 11)))/100
    else if (geopot_elev <= 32)
	(5474.889 * math.pow(216.65 / t, 34.16319))/100
    else if (geopot_elev <= 47)
	 (868.0187 * math.pow(228.65 / t, 12.2011))/100
    else if (geopot_elev <= 51)
	(110.9063 * math.exp(-0.1262 * (geopot_elev - 47)))/100
    else if (geopot_elev <= 71)
	(66.93887 * math.pow(270.65 / t, -12.2011))/100
    else if (geopot_elev <= 84.85)
	(3.956420 * math.pow(214.65 / t, -17.0816))/100
    else
        (101325 * math.pow(288.15 / t, -5.255877))/100
   
}



//weather condition wrt hum,temp,press
  def getCond(humidity:Int,temperature:Int,pressure:Int):String = {
    if (temperature > 0 && humidity < 90 )
        "Sunny"
    else if (temperature == 0 && humidity > 95)
	      "Rain"
    else if (temperature <= 0)
        "Snow"
    else if  (humidity >= 90 && temperature > 0 && pressure < 1000)
        "Rain"
    else if (humidity < 90 && temperature > 0 && pressure < 1000)
        "Cloudy"
    else
        "Sunny"
  }


} // end of class




object simulateWeather extends App{
//   def main(args: Array[String]) {
  val w = new Weather;

  val cnt = w.city.length - 1
  val filename=scala.io.StdIn.readLine("Enter File Name: ") //"weatherdata.txt"
//val outfile=new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename)))
  val outfile=scala.tools.nsc.io.File(filename)
  if (outfile.exists)
    {
      println("File exists with name "+filename)
      System.exit(0)
    }
for ( i <- 0 to cnt) {
    var cntr=new Random
    val ind=cntr.nextInt( (cnt - 1) + 1 )
    val e=w.getElve
    val t=w.getTemp(e).toInt
    val p=w.getPress(e).toInt

//Longitude betwn -180 to + 180 & Latitude betwn -90 to +90.pass the lat/long range
  var recStr=w.city(ind)+"|"+w.getLongLati(90)+","+w.getLongLati(180)+","+e+"|"+w.getDateTime+"|"+w.getCond(t,p,e)+"|"+w.truncTo(w.getTemp(e),2)+"|" +w.truncTo(w.getPress(e),2)+"|"+w.getElve

  outfile.appendAll(recStr+"\n")
//outfile.write(recStr+"\n")
  } //end of for
 //outfile.close()
  println("Data Simulation Done")
//} //end of main
} //end of obj
