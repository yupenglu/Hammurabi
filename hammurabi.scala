import scala.util.Random
import scala.util.control.Breaks._  // in order to use "break" in a loop later

object Hammurabi{
  def printIntroductoryMessage = {
  println("""
  Congratulations, you are the newest ruler of ancient Samaria,
  elected for a ten year term of office. Your duties are to dispense food,
  direct farming, and buy and sell land as needed to support your people.
  Watch out for rat infestations and the plague! Grain is the general currency,
  measured in bushels. The following will help you in your decisions:

  * Each person needs at least 20 bushels of grain per year to survive.

  * Each person can farm at most 10 acres of land.

  * It takes 2 bushels of grain to farm an acre of land.

  * The market price for land fluctuates yearly.

  Rule wisely and you will be showered with appreciation at the end of
  your term. Rule poorly and you will be kicked out of office!
  """)
  }
  def hammurabi = {
    var starved = 0            // how many people starved
    var immigrants = 5         // how many people came to the city
    var population = 100
    var harvest = 3000          // total bushels harvested
    var bushelsPerAcre = 3      // amount harvested for each acre planted
    var rats_ate = 200          // bushels destroyed by rats
    var bushelsInStorage = 2800
    var acresOwned = 1000
    var pricePerAcre = 19       // each acre costs this many bushels
    var plagueDeaths = 0
    var year = 1
    var gameEnds = false        // game ends when it is true
    var thrownOut = false       // record whether 45% are starved or not
    var percent = 0             // helps to calculator the percentage of
                                 // people starved to death 
    printIntroductoryMessage
    
    
    breakable{                    // required to use "break" later
    while (year <= 10) {             // year loop
      println("O great Hammurabi!")
      println("You are in year " + year + " of your ten year rule.")
      println("In the previous year " +
      starved + " people starved to death.")
      println("In the previous year " +
      immigrants + " people entered the kingdom.")
      println("The population is now " + population)
      println("We harvested " + harvest + " bushels at " + bushelsPerAcre +
      " bushels per acre.")
      println("Rats destroyed " + rats_ate + " bushels, leaving " +
      bushelsInStorage + " bushels in storage.")
      println("The city owns " + acresOwned + " acres of land.")
      println("Land is currently worth " + pricePerAcre + " bushels per acre.")
      println("There were " + plagueDeaths + " deaths from the plague.")
      println   // add an empty line
      
      def readInt(message: String): Int = {   
        try {
          readLine(message).toInt
        } catch {
          case _ : Throwable =>
            println("That's not an integer. Please enter an integer.")
          readInt(message)
        }
      }                // a method of input Int preventing program fro crashing
      def askHowMuchLandToBuy(bushels: Int, price: Int) = {
        var acresToBuy = readInt("How many acres will you buy? ")
        while (acresToBuy < 0 || acresToBuy * price > bushels) {
          println("O Great Hammurabi, we have but " + bushels +
          " bushels of grain!")
          println()
          acresToBuy = readInt("How many acres will you buy? ")
        }
        acresToBuy
      }
      def askHowMuchLandToSell(acres: Int) = {
        println
        var acresToSell = readInt("How many acres will you sell? ")
        while (acresToSell < 0 || acresToSell > acres) {
          println("O Great Hammurabi, we have but " + acres +
          " acres of land!")
          println()
          acresToSell = readInt("How many acres will you sell? ")
        }
        acresToSell
      }
      def askHowMuchGrainToFeed(bushels: Int) = {
        println
        var bushelsToFeed = readInt("How much grain to feed to the people? ")
        while (bushelsToFeed < 0 || bushelsToFeed > bushels) {
          println("O Great Hammurabi, we have but " + bushels +
          " bushels of grain!")
          println()
          bushelsToFeed = readInt("How much grain to feed to the people? ")
        }
        bushelsToFeed
      }      
      def askHowManyAcresToPlant(bushels: Int, acres: Int, population: Int) = {
        println
        var acresToPlant = readInt("How many acres to plant with seed? ")
        while (acresToPlant < 0 || acresToPlant > acres ||
        acresToPlant > bushels / 2 || acresToPlant > 10 * population) {
          println()
          println("O Great Hammurabi, we have only " + population +
          " people to work, " + acres + " acres to plant, and " +
          bushels + " bushels of grain to seed!")
          println()
          acresToPlant = readInt("How many acres to plant with seed? ")
        }
        acresToPlant
      }            
      
      
      var acresToBuy = askHowMuchLandToBuy(bushelsInStorage, pricePerAcre)
      acresOwned = acresOwned + acresToBuy
      bushelsInStorage = bushelsInStorage - acresToBuy * pricePerAcre   // buy
     
      if (acresToBuy == 0){
        var acresToSell = askHowMuchLandToSell(acresOwned)
        acresOwned = acresOwned - acresToSell
        bushelsInStorage = bushelsInStorage + acresToSell * pricePerAcre 
      }                                                                 // sell
      
      var bushelsToFeed = askHowMuchGrainToFeed(bushelsInStorage)
      bushelsInStorage = bushelsInStorage - bushelsToFeed     // feed
      
      var acresToPlant = askHowManyAcresToPlant(bushelsInStorage, acresOwned,
      population)
      bushelsInStorage = bushelsInStorage - acresToPlant * 2    
                                                         // input section ends
      def diedFromPlague(population: Int) = {
        if (Random.nextInt(100) < 15) {
        population / 2
        } else 0
      }
      plagueDeaths = diedFromPlague(population)
      population = population - plagueDeaths       // plague
      
      def diedFromStarve(population: Int, bushels: Int) = {
        var starvedToDeath = 0
        if (population * 20 > bushels) {
          starvedToDeath = population - bushels / 20
        } else {
          starvedToDeath = 0
        }
        if (starvedToDeath > (0.45 * population).toInt) {gameEnds = true}
        starvedToDeath
      }
      starved = diedFromStarve(population, bushelsToFeed)
      percent = starved * 100 / population
      population = population - starved         // starvation
      
      if (gameEnds == true) {
        thrownOut = true
        break()
      }                              // if more than 45% starved, kick out
          
      if (starved == 0) {
        immigrants = ((20 * acresOwned + bushelsInStorage) /
        (100 * population) + 1)
        population = population + immigrants
      } else {
        immigrants = 0
      }                                 // new residents
      
      bushelsPerAcre = (Random.nextInt(8) + 1)
      harvest = bushelsPerAcre * acresToPlant
      bushelsInStorage = bushelsInStorage + harvest     // harvest
      
      def rats(bushels: Int) = {
        if (Random.nextInt(100) < 40) {
        (Random.nextInt(3) + 1) * bushels / 10
        } else 0
      }     
      rats_ate = rats(bushelsInStorage)
      bushelsInStorage = bushelsInStorage - rats_ate      // rats
      
      pricePerAcre = Random.nextInt(6) + 17     // each year a new price
            
      year = year + 1
      println()                         // add some blank line for tidiness
      println()
    }
    }
    
    if (thrownOut == true) {              // starts of summary section
      println()
      println()
      println("Oh my!! More than " + percent +
      "% of people starved to death in the past year. " +
      "You are osted by your people!!")
      println("Game Over!!")
      println()
    } else if (population >= 1000) {
      println("""
      Well done!! You have completed your 10-year term of being a ruler.
      During the past ten years, """ + (population - 100) +
      " people move to your country!")
      println()
    } else if (population < 1000) {
      println("""
      Uh..I cannot say that you are a good ruler. In the past 10 years,
      your city experienced a negative population growth of """ +
      (100 - population) + """ people.
      You have to apologize to your people!""")
      println()                                     
    }
  }
}
Hammurabi.hammurabi
