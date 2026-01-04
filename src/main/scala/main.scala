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
    println(s"${CYAN}2.${RESET} Continue Saved Game")
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

  // Single player mode
  def playSinglePlayer(): Unit = {
    println(s"\n${GREEN}=== SINGLE PLAYER MODE ===${RESET}")
    val player = Player.createNewPlayer(1)
    val word = WordList.getRandomWord()
    
    println(s"${YELLOW}A random word has been selected!${RESET}")
    val won = GameEngine.playGame(word, player.name)
    
    // Update player stats (this will be saved to scoreboard later)
    val updatedPlayer = player.addGameResult(won)
    
    // TODO: Save to scoreboard
    println(s"\n${CYAN}Your stats: ${updatedPlayer.gamesPlayed} games played, ${updatedPlayer.gamesWon} won${RESET}")
    
    // Ask if they want to play again
    pressEnterToContinue()
  }

  // Multiplayer mode
  def playMultiplayer(): Unit = {
    println(s"\n${GREEN}=== MULTIPLAYER MODE ===${RESET}")
    
    // Get Player 1 (word setter)
    val player1 = Player.createNewPlayer(1)
    
    // Get Player 2 (guesser)
    val player2 = Player.createNewPlayer(2)
    
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
    val won = GameEngine.playGame(word, player2.name)
    
    // Update player stats
    val updatedPlayer2 = player2.addGameResult(won)
    
    // TODO: Save to scoreboard
    println(s"\n${CYAN}${player2.name}'s stats: ${updatedPlayer2.gamesPlayed} games played, ${updatedPlayer2.gamesWon} won${RESET}")
    
    pressEnterToContinue()
  }

  // Continue saved game (placeholder for now)
  def continueSavedGame(): Unit = {
    println(s"\n${YELLOW}=== CONTINUE SAVED GAME ===${RESET}")
    println(s"${RED}Save/Load functionality coming soon!${RESET}")
    // TODO: Implement load game from FileManager
    pressEnterToContinue()
  }

  // View scoreboard (placeholder for now)
  def viewScoreboard(): Unit = {
    println(s"\n${YELLOW}=== SCOREBOARD ===${RESET}")
    println(s"${RED}Scoreboard functionality coming soon!${RESET}")
    // TODO: Load and display scoreboard from file
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
          println(s"\n${GREEN}Thanks for playing Hangman! Goodbye!${RESET}\n")
          running = false
      }
    }
  }
}