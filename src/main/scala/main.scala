object HangmanGame {

  val RESET = "\u001B[0m"
  val RED = "\u001B[31m"
  val GREEN = "\u001B[32m"
  val YELLOW = "\u001B[33m"
  val BLUE = "\u001B[34m"
  val CYAN = "\u001B[36m"

  // Game state case class
  case class GameState(
                        word: String,
                        guessedLetters: Set[Char],
                        remainingAttempts: Int,
                        maxAttempts: Int = 6
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
    println(s"${CYAN}HANGMAN GAME${RESET}")
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
  def getLetterInput(): Option[Char] = {
    print(s"${CYAN}Enter a letter: ${RESET}")
    val input = scala.io.StdIn.readLine().trim

    if (input.length == 1 && input.head.isLetter) {
      Some(input.head)
    } else {
      println(s"${RED}Invalid input! Please enter a single letter.${RESET}")
      None
    }
  }

  // Main game loop
  def playGame(initialWord: String): Unit = {
    var state = GameState(
      word = initialWord,
      guessedLetters = Set.empty,
      remainingAttempts = 6
    )

    println(s"\n${GREEN}Game started! The word has ${initialWord.length} letters.${RESET}")

    while (!state.isGameOver) {
      displayGameState(state)

      getLetterInput() match {
        case Some(letter) =>
          val (newState, message) = processGuess(state, letter)
          state = newState
          println(message)

        case None =>
        // Invalid input, continue loop
      }
    }

    // Display final state
    displayGameState(state)

    // Display game result
    if (state.isWordGuessed) {
      println(s"${GREEN}╔════════════════════════════════════════╗${RESET}")
      println(s"${GREEN}║   🎉 CONGRATULATIONS! YOU WON! 🎉    ║${RESET}")
      println(s"${GREEN}╚════════════════════════════════════════╝${RESET}")
      println(s"${YELLOW}The word was: ${state.word}${RESET}")
    } else {
      println(s"${RED}╔════════════════════════════════════════╗${RESET}")
      println(s"${RED}║        💀 GAME OVER! YOU LOST! 💀      ║${RESET}")
      println(s"${RED}╚════════════════════════════════════════╝${RESET}")
      println(s"${YELLOW}The word was: ${state.word}${RESET}")
    }
  }

  // Test function - will be replaced by your colleague's menu system
  def main(args: Array[String]): Unit = {
    println(s"${BLUE}╔════════════════════════════════════════╗${RESET}")
    println(s"${BLUE}║          WELCOME TO HANGMAN!           ║${RESET}")
    println(s"${BLUE}╚════════════════════════════════════════╝${RESET}")

    // Sample words for testing (> 5 letters)
    val sampleWords = Array("programming", "computer", "algorithm", "function", "variable")
    val randomWord = sampleWords(scala.util.Random.nextInt(sampleWords.length))

    playGame(randomWord)
  }
}