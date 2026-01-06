// Main.scala
// This is the entry point of the game with menu system

object Main {
  
  import GameEngine._

  // Display the main menu
  def displayMainMenu(): Unit = {
    println("\n" + "="*50)
    println(s"${BLUE}╔═══════════════════════════════════════╗${RESET}")
    println(s"${BLUE}║          WELCOME TO HANGMAN!          ║${RESET}")
    println(s"${BLUE}╚═══════════════════════════════════════╝${RESET}")
    println("="*50)
    println(s"${CYAN}1.${RESET} Start New Game")
    
    // Show if saved game exists
    if (FileManager.hasSavedGame()) {
      println(s"${CYAN}2.${RESET} Continue Saved Game ${GREEN}[Available]${RESET}")
    } else {
      println(s"${CYAN}2.${RESET} Continue Saved Game ${RED}[No Save Found]${RESET}")
    }
    
    println(s"${CYAN}3.${RESET} View Scoreboard")
    println(s"${CYAN}4.${RESET} Exit")
    println("="*50)
  }

  // Display game mode selection menu
  def displayGameModeMenu(): Unit = {
    println("\n" + "="*50)
    println(s"${YELLOW}SELECT GAME MODE${RESET}")
    println("="*50)
    println(s"${CYAN}1.${RESET} Single Player (vs Computer)")
    println(s"${CYAN}2.${RESET} Multiplayer (Player 1 vs Player 2)")
    println(s"${CYAN}3.${RESET} Back to Main Menu")
    println("="*50)
  }

  // Get user menu choice
  def getMenuChoice(maxOption: Int): Int = {
    print(s"${CYAN}Enter your choice (1-$maxOption): ${RESET}")
    try {
      val choice = scala.io.StdIn.readLine().trim.toInt
      if (choice >= 1 && choice <= maxOption) choice
      else {
        println(s"${RED}Invalid choice! Please enter a number between 1 and $maxOption.${RESET}")
        getMenuChoice(maxOption)
      }
    } catch {
      case _: Exception =>
        println(s"${RED}Invalid input! Please enter a number.${RESET}")
        getMenuChoice(maxOption)
    }
  }

  // Start a new game
  def startNewGame(): Unit = {
    displayGameModeMenu()
    val mode = getMenuChoice(3)

    mode match {
      case 1 => playSinglePlayer()
      case 2 => playMultiplayer()
      case 3 => // Return to main menu
    }
  }

  // Get or create player from scoreboard
  def getOrCreatePlayer(playerNumber: Int): Player = {
    print(s"${CYAN}Enter name for Player $playerNumber: ${RESET}")
    val name = scala.io.StdIn.readLine().trim
    
    if (name.isEmpty) {
      println(s"${YELLOW}Name cannot be empty. Using default name.${RESET}")
      Player(s"Player$playerNumber")
    } else {
      // Check if player exists in scoreboard
      FileManager.getPlayer(name) match {
        case Some(existingPlayer) =>
          println(s"${GREEN}Welcome back, ${existingPlayer.name}!${RESET}")
          println(s"${YELLOW}Your record: ${existingPlayer.gamesWon}/${existingPlayer.gamesPlayed} wins${RESET}")
          existingPlayer
        case None =>
          println(s"${GREEN}New player registered: $name${RESET}")
          Player(name)
      }
    }
  }

  // Single player mode
  def playSinglePlayer(): Unit = {
    println(s"\n${GREEN}=== SINGLE PLAYER MODE ===${RESET}")
    val player = getOrCreatePlayer(1)
    val word = WordList.getRandomWord()
    
    println(s"${YELLOW}A random word has been selected!${RESET}")
    
    GameEngine.playGame(word, player.name) match {
      case Some(won) =>
        // Game finished (not saved)
        val updatedPlayer = player.addGameResult(won)
        FileManager.updatePlayerScore(updatedPlayer)
        
        println(s"\n${CYAN}=== YOUR STATS ===${RESET}")
        updatedPlayer.displayStats()
        
      case None =>
        // Game was saved
        println(s"${YELLOW}Your progress has been saved. Continue from the main menu!${RESET}")
    }
    
    pressEnterToContinue()
  }

  // Multiplayer mode
  def playMultiplayer(): Unit = {
    println(s"\n${GREEN}=== MULTIPLAYER MODE ===${RESET}")
    
    // Get Player 1 (word setter)
    val player1 = getOrCreatePlayer(1)
    
    // Get Player 2 (guesser)
    val player2 = getOrCreatePlayer(2)
    
    // Player 1 enters the word
    println(s"\n${YELLOW}${player1.name}, enter a word for ${player2.name} to guess.${RESET}")
    println(s"${RED}(Make sure ${player2.name} is not looking!)${RESET}")
    
    var word = ""
    var validWord = false
    
    while (!validWord) {
      print(s"${CYAN}Enter word (must be > 5 letters): ${RESET}")
      word = scala.io.StdIn.readLine().trim.toLowerCase
      
      if (WordList.isValidWord(word)) {
        validWord = true
        // Clear screen to hide the word
        println("\n" * 50)
        println(s"${GREEN}Word accepted! ${player2.name}, get ready to guess!${RESET}")
      } else {
        println(s"${RED}Invalid word! Must be longer than 5 letters and contain only letters.${RESET}")
      }
    }
    
    // Player 2 plays the game
    GameEngine.playGame(word, player2.name) match {
      case Some(won) =>
        // Game finished
        val updatedPlayer2 = player2.addGameResult(won)
        FileManager.updatePlayerScore(updatedPlayer2)
        
        println(s"\n${CYAN}=== ${player2.name}'S STATS ===${RESET}")
        updatedPlayer2.displayStats()
        
      case None =>
        // Game was saved
        println(s"${YELLOW}Game saved! ${player2.name} can continue later.${RESET}")
    }
    
    pressEnterToContinue()
  }

  // Continue saved game
  def continueSavedGame(): Unit = {
    println(s"\n${YELLOW}=== CONTINUE SAVED GAME ===${RESET}")
    
    if (!FileManager.hasSavedGame()) {
      println(s"${RED}No saved game found!${RESET}")
      pressEnterToContinue()
      return
    }
    
    // Load the player name BEFORE starting the game (because the save file gets deleted after game ends)
    val playerNameOpt = FileManager.loadGame().map(_.playerName)
    
    GameEngine.continueSavedGame() match {
      case Some(won) =>
        // Game finished - update scoreboard
        playerNameOpt match {
          case Some(playerName) =>
            FileManager.getPlayer(playerName) match {
              case Some(player) =>
                val updatedPlayer = player.addGameResult(won)
                FileManager.updatePlayerScore(updatedPlayer)
                println(s"\n${CYAN}=== UPDATED STATS ===${RESET}")
                updatedPlayer.displayStats()
              case None =>
                // Player not in scoreboard yet
                val newPlayer = Player(playerName).addGameResult(won)
                FileManager.updatePlayerScore(newPlayer)
                println(s"\n${CYAN}=== UPDATED STATS ===${RESET}")
                newPlayer.displayStats()
            }
          case None =>
            println(s"${RED}Error: Could not retrieve player information.${RESET}")
        }
        
      case None =>
        // Game was saved again or failed to load
    }
    
    pressEnterToContinue()
  }

  // View scoreboard
  def viewScoreboard(): Unit = {
    Scoreboard.displayScoreboard()
    pressEnterToContinue()
  }

  // Helper function to pause and wait for user
  def pressEnterToContinue(): Unit = {
    print(s"\n${CYAN}Press ENTER to continue...${RESET}")
    scala.io.StdIn.readLine()
  }

  // Main entry point
  def main(args: Array[String]): Unit = {
    var running = true

    while (running) {
      displayMainMenu()
      val choice = getMenuChoice(4)

      choice match {
        case 1 => startNewGame()
        case 2 => continueSavedGame()
        case 3 => viewScoreboard()
        case 4 =>
          println(s"\n${GREEN}╔═══════════════════════════════════════╗${RESET}")
          println(s"${GREEN}║    Thanks for playing Hangman! 🎮     ║${RESET}")
          println(s"${GREEN}╚═══════════════════════════════════════╝${RESET}\n")
          running = false
      }
    }
  }
}