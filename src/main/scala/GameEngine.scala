// This file contains the core game logic for Hangman

object GameEngine {

  // Color codes for terminal output
  val RESET = "\u001B[0m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val CYAN = "\u001B[36m"

  // Game state case class - represents the current state of a game
  case class GameState(
                        word: String,                    // The word to guess
                        guessedLetters: Set[Char],       // Letters already guessed
                        remainingAttempts: Int,          // How many wrong guesses left
                        maxAttempts: Int = 6,            // Total allowed wrong guesses
                        playerName: String = ""          // Name of the player
                      ) {

    // Get the current display of the word (with underscores for unguessed letters)
    def getWordDisplay: String = {
      word.map(c => if (guessedLetters.contains(c.toLower)) c else '_').mkString(" ")
    }

    // Check if the word is completely guessed
    def isWordGuessed: Boolean = {
      word.toLowerCase.forall(c => guessedLetters.contains(c))
    }

    // Check if game is lost
    def isGameLost: Boolean = remainingAttempts <= 0

    // Check if game is over
    def isGameOver: Boolean = isWordGuessed || isGameLost

    // Get incorrect guesses
    def getIncorrectGuesses: Set[Char] = {
      guessedLetters.filterNot(c => word.toLowerCase.contains(c))
    }
  }

  // ASCII art for hangman at different stages
  def getHangmanArt(incorrectGuesses: Int): String = {
    val stages = Array(
      """
        |   +---+
        |   |   |
        |       |
        |       |
        |       |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |       |
        |       |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |   |   |
        |       |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |  /|   |
        |       |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |  /|\  |
        |       |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |  /|\  |
        |  /    |
        |       |
        | =========""",

      """
        |   +---+
        |   |   |
        |   O   |
        |  /|\  |
        |  / \  |
        |       |
        | =========""",
    )

    if (incorrectGuesses >= 0 && incorrectGuesses < stages.length) {
      stages(incorrectGuesses)
    } else {
      stages.last
    }
  }

  // Display the current game state
  def displayGameState(state: GameState): Unit = {
    println("\n" + "="*50)
    println(s"${CYAN}HANGMAN GAME - Player: ${state.playerName}${RESET}")
    println("="*50)

    // Display hangman
    println(s"${RED}${getHangmanArt(state.getIncorrectGuesses.size)}${RESET}")

    // Display word status
    println(s"\n${YELLOW}Word: ${state.getWordDisplay}${RESET}")

    // Display guessed letters
    if (state.guessedLetters.nonEmpty) {
      println(s"${BLUE}Guessed letters: ${state.guessedLetters.toList.sorted.mkString(", ")}${RESET}")
    }

    // Display remaining attempts
    println(s"${GREEN}Remaining attempts: ${state.remainingAttempts}${RESET}")
    println("="*50 + "\n")
  }

  // Validate and process a guess
  def processGuess(state: GameState, guess: Char): (GameState, String) = {
    val lowerGuess = guess.toLower

    // Check if already guessed
    if (state.guessedLetters.contains(lowerGuess)) {
      return (state, s"${YELLOW}You already guessed '$guess'. Try a different letter.${RESET}")
    }

    // Add to guessed letters
    val newGuessedLetters = state.guessedLetters + lowerGuess

    // Check if guess is correct
    if (state.word.toLowerCase.contains(lowerGuess)) {
      val newState = state.copy(guessedLetters = newGuessedLetters)
      (newState, s"${GREEN}Correct! '$guess' is in the word.${RESET}")
    } else {
      val newState = state.copy(
        guessedLetters = newGuessedLetters,
        remainingAttempts = state.remainingAttempts - 1
      )
      (newState, s"${RED}Wrong! '$guess' is not in the word.${RESET}")
    }
  }

  // Get a valid letter input from user
  def getLetterInput(): Either[String, Char] = {
    print(s"${CYAN}Enter a letter (or type 'save' to save and quit): ${RESET}")
    val input = scala.io.StdIn.readLine().trim.toLowerCase

    if (input == "save") {
      Left("save") // Signal to save game
    } else if (input.length == 1 && input.head.isLetter) {
      Right(input.head) // Valid letter
    } else {
      println(s"${RED}Invalid input! Please enter a single letter or 'save'.${RESET}")
      Left("invalid") // Invalid input, continue loop
    }
  }

  // Helper function to display end game and return result
  private def displayEndGame(state: GameState): Boolean = {
    displayGameState(state)
    
    if (state.isWordGuessed) {
      println(s"${GREEN}╔═══════════════════════════════════════╗${RESET}")
      println(s"${GREEN}║   🎉 CONGRATULATIONS! YOU WON! 🎉    ║${RESET}")
      println(s"${GREEN}╚═══════════════════════════════════════╝${RESET}")
      println(s"${YELLOW}The word was: ${state.word}${RESET}")
      true // Player won
    } else {
      println(s"${RED}╔═══════════════════════════════════════╗${RESET}")
      println(s"${RED}║        💀 GAME OVER! YOU LOST! 💀      ║${RESET}")
      println(s"${RED}╚═══════════════════════════════════════╝${RESET}")
      println(s"${YELLOW}The word was: ${state.word}${RESET}")
      false // Player lost
    }
  }

  // Main game loop - returns Option[Boolean]: Some(true) if won, Some(false) if lost, None if saved
  def playGame(initialWord: String, playerName: String): Option[Boolean] = {
    var state = GameState(
      word = initialWord,
      guessedLetters = Set.empty,
      remainingAttempts = 6,
      playerName = playerName
    )

    println(s"\n${GREEN}Game started! The word has ${initialWord.length} letters.${RESET}")
    println(s"${YELLOW}Tip: Type 'save' at any time to save your progress and quit.${RESET}")

    while (!state.isGameOver) {
      displayGameState(state)

      getLetterInput() match {
        case Right(letter) =>
          val (newState, message) = processGuess(state, letter)
          state = newState
          println(message)

        case Left("save") =>
          // Save the game
          if (FileManager.saveGame(state)) {
            println(s"${GREEN}Game saved! You can continue later from the main menu.${RESET}")
            return None // Game was saved, not finished
          } else {
            println(s"${RED}Failed to save game. Continuing...${RESET}")
          }

        case Left(_) =>
          // Invalid input, continue loop
      }
    }

    // Display final result using helper function
    Some(displayEndGame(state))
  }

  // Continue a saved game
  def continueSavedGame(): Option[Boolean] = {
    FileManager.loadGame() match {
      case Some(savedState) =>
        println(s"\n${GREEN}Resuming saved game for ${savedState.playerName}...${RESET}")
        println(s"${YELLOW}Word length: ${savedState.word.length} letters${RESET}")
        println(s"${YELLOW}Progress: ${savedState.getWordDisplay}${RESET}")
        
        // Continue the game loop with saved state
        var state = savedState

        while (!state.isGameOver) {
          displayGameState(state)

          getLetterInput() match {
            case Right(letter) =>
              val (newState, message) = processGuess(state, letter)
              state = newState
              println(message)

            case Left("save") =>
              if (FileManager.saveGame(state)) {
                println(s"${GREEN}Game saved!${RESET}")
                return None
              }

            case Left(_) =>
              // Invalid input, continue
          }
        }

        // Game finished - delete save file and display result
        FileManager.deleteSaveFile()
        Some(displayEndGame(state))

      case None =>
        println(s"${RED}No saved game found or failed to load.${RESET}")
        None
    }
  }
}