// Displays and manages the player scoreboard

object Scoreboard {
  
  import GameEngine._

  // Display the full scoreboard
  def displayScoreboard(): Unit = {
    val players = FileManager.loadScoreboard()
    
    if (players.isEmpty) {
      println(s"\n${YELLOW}No players on the scoreboard yet!${RESET}")
      println(s"${CYAN}Play some games to see your scores here.${RESET}\n")
      return
    }

    // Sort players by win rate (descending), then by games won
    val sortedPlayers = players.sortBy(p => (-p.winPercentage, -p.gamesWon))

    println("\n" + "="*60)
    println(s"${CYAN}╔════════════════════════════════════════════════════════╗${RESET}")
    println(s"${CYAN}║                    🏆 SCOREBOARD 🏆                    ║${RESET}")
    println(s"${CYAN}╚════════════════════════════════════════════════════════╝${RESET}")
    println("="*60)
    
    // Header
    println(f"${YELLOW}${"Rank"}%-6s ${"Player Name"}%-20s ${"Games"}%-8s ${"Wins"}%-8s ${"Win Rate"}%-10s${RESET}")
    println("-"*60)

    // Display each player
    sortedPlayers.zipWithIndex.foreach { case (player, index) =>
      val rank = index + 1
      val medal = rank match {
        case 1 => "🥇"
        case 2 => "🥈"
        case 3 => "🥉"
        case _ => s"$rank."
      }
      
      val color = rank match {
        case 1 => YELLOW  // Gold
        case 2 => CYAN    // Silver
        case 3 => RED     // Bronze
        case _ => GREEN   // Others
      }

      println(f"${color}${medal}%-6s ${player.name}%-20s ${player.gamesPlayed}%-8d ${player.gamesWon}%-8d ${player.winPercentage}%.1f%%${RESET}")
    }
    
    println("="*60)
    println(s"${CYAN}Total Players: ${players.length}${RESET}\n")
  }

  // Display a specific player's stats
  def displayPlayerStats(playerName: String): Unit = {
    FileManager.getPlayer(playerName) match {
      case Some(player) =>
        println("\n" + "="*50)
        println(s"${CYAN}Player Stats for: ${player.name}${RESET}")
        println("="*50)
        player.displayStats()
        println("="*50 + "\n")
      case None =>
        println(s"${RED}Player '$playerName' not found in scoreboard.${RESET}")
    }
  }

  // Get top N players
  def getTopPlayers(n: Int): List[Player] = {
    val players = FileManager.loadScoreboard()
    players.sortBy(p => (-p.winPercentage, -p.gamesWon)).take(n)
  }

  // Clear the entire scoreboard (useful for testing)
  def clearScoreboard(): Unit = {
    print(s"${RED}Are you sure you want to clear the entire scoreboard? (yes/no): ${RESET}")
    val response = scala.io.StdIn.readLine().trim.toLowerCase
    
    if (response == "yes") {
      FileManager.saveScoreboard(List.empty)
      println(s"${GREEN}Scoreboard cleared successfully!${RESET}")
    } else {
      println(s"${YELLOW}Scoreboard clearing cancelled.${RESET}")
    }
  }
}