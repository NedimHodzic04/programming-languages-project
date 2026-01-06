// Handles saving and loading game data to/from JSON files

import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*
import java.nio.file.{Files, Paths}
import scala.util.{Try, Success, Failure}

// Data structure for saved game
case class SavedGame(
  word: String,
  guessedLetters: List[Char],
  remainingAttempts: Int,
  playerName: String,
  timestamp: Long = System.currentTimeMillis()
)

object FileManager {
  
  private val SAVE_FILE = "hangman_save.json"
  private val SCOREBOARD_FILE = "scoreboard.json"

  // Convert GameState to SavedGame
  def gameStateToSaved(state: GameEngine.GameState): SavedGame = {
    SavedGame(
      word = state.word,
      guessedLetters = state.guessedLetters.toList,
      remainingAttempts = state.remainingAttempts,
      playerName = state.playerName
    )
  }

  // Convert SavedGame back to GameState
  def savedToGameState(saved: SavedGame): GameEngine.GameState = {
    GameEngine.GameState(
      word = saved.word,
      guessedLetters = saved.guessedLetters.toSet,
      remainingAttempts = saved.remainingAttempts,
      playerName = saved.playerName
    )
  }

  // Save game state to JSON file
  def saveGame(state: GameEngine.GameState): Boolean = {
    try {
      val savedGame = gameStateToSaved(state)
      val json = savedGame.asJson.spaces2
      Files.write(Paths.get(SAVE_FILE), json.getBytes)
      println(s"${GameEngine.GREEN}Game saved successfully!${GameEngine.RESET}")
      true
    } catch {
      case e: Exception =>
        println(s"${GameEngine.RED}Error saving game: ${e.getMessage}${GameEngine.RESET}")
        false
    }
  }

  // Load game state from JSON file
  def loadGame(): Option[GameEngine.GameState] = {
    try {
      if (!Files.exists(Paths.get(SAVE_FILE))) {
        println(s"${GameEngine.YELLOW}No saved game found.${GameEngine.RESET}")
        return None
      }

      val json = new String(Files.readAllBytes(Paths.get(SAVE_FILE)))
      decode[SavedGame](json) match {
        case Right(savedGame) =>
          println(s"${GameEngine.GREEN}Game loaded successfully!${GameEngine.RESET}")
          Some(savedToGameState(savedGame))
        case Left(error) =>
          println(s"${GameEngine.RED}Error loading game: ${error.getMessage}${GameEngine.RESET}")
          None
      }
    } catch {
      case e: Exception =>
        println(s"${GameEngine.RED}Error reading save file: ${e.getMessage}${GameEngine.RESET}")
        None
    }
  }

  // Check if a saved game exists
  def hasSavedGame(): Boolean = {
    Files.exists(Paths.get(SAVE_FILE))
  }

  // Delete saved game file
  def deleteSaveFile(): Unit = {
    try {
      if (Files.exists(Paths.get(SAVE_FILE))) {
        Files.delete(Paths.get(SAVE_FILE))
        println(s"${GameEngine.GREEN}Save file deleted.${GameEngine.RESET}")
      }
    } catch {
      case e: Exception =>
        println(s"${GameEngine.RED}Error deleting save file: ${e.getMessage}${GameEngine.RESET}")
    }
  }

  // Save scoreboard (list of players) to JSON
  def saveScoreboard(players: List[Player]): Boolean = {
    try {
      val json = players.asJson.spaces2
      Files.write(Paths.get(SCOREBOARD_FILE), json.getBytes)
      true
    } catch {
      case e: Exception =>
        println(s"${GameEngine.RED}Error saving scoreboard: ${e.getMessage}${GameEngine.RESET}")
        false
    }
  }

  // Load scoreboard from JSON
  def loadScoreboard(): List[Player] = {
    try {
      if (!Files.exists(Paths.get(SCOREBOARD_FILE))) {
        return List.empty
      }

      val json = new String(Files.readAllBytes(Paths.get(SCOREBOARD_FILE)))
      decode[List[Player]](json) match {
        case Right(players) => players
        case Left(error) =>
          println(s"${GameEngine.RED}Error loading scoreboard: ${error.getMessage}${GameEngine.RESET}")
          List.empty
      }
    } catch {
      case e: Exception =>
        println(s"${GameEngine.RED}Error reading scoreboard file: ${e.getMessage}${GameEngine.RESET}")
        List.empty
    }
  }

  // Update a player's record in the scoreboard
  def updatePlayerScore(player: Player): Unit = {
    val scoreboard = loadScoreboard()
    
    // Check if player already exists
    val existingPlayerIndex = scoreboard.indexWhere(_.name == player.name)
    
    val updatedScoreboard = if (existingPlayerIndex >= 0) {
      // Update existing player
      scoreboard.updated(existingPlayerIndex, player)
    } else {
      // Add new player
      scoreboard :+ player
    }
    
    saveScoreboard(updatedScoreboard)
  }

  // Get a specific player from scoreboard
  def getPlayer(name: String): Option[Player] = {
    loadScoreboard().find(_.name == name)
  }
}