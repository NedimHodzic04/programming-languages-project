// This file manages player information and scores

case class Player(
                   name: String,              // Player's name
                   gamesPlayed: Int = 0,      // Total games played
                   gamesWon: Int = 0          // Total games won
                 ) {
  
  // Calculate win percentage
  def winPercentage: Double = {
    if (gamesPlayed == 0) 0.0
    else (gamesWon.toDouble / gamesPlayed.toDouble) * 100.0
  }

  // Add a game result to this player's record
  def addGameResult(won: Boolean): Player = {
    if (won) {
      this.copy(gamesPlayed = gamesPlayed + 1, gamesWon = gamesWon + 1)
    } else {
      this.copy(gamesPlayed = gamesPlayed + 1)
    }
  }

  // Display player stats
  def displayStats(): Unit = {
    println(s"  Player: $name")
    println(s"  Games Played: $gamesPlayed")
    println(s"  Games Won: $gamesWon")
    println(s"  Win Rate: ${f"$winPercentage%.1f"}%")
  }
}

object Player {
  // Create a new player by asking for their name
  def createNewPlayer(playerNumber: Int): Player = {
    print(s"${GameEngine.CYAN}Enter name for Player $playerNumber: ${GameEngine.RESET}")
    val name = scala.io.StdIn.readLine().trim
    
    if (name.isEmpty) {
      println(s"${GameEngine.YELLOW}Name cannot be empty. Using default name.${GameEngine.RESET}")
      Player(s"Player$playerNumber")
    } else {
      Player(name)
    }
  }
}