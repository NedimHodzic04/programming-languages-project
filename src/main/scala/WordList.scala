// This file manages the word list for single player mode
// All words must be longer than 5 letters

object WordList {
  
  // List of words for the game (all > 5 letters)
  private val words = Array(
    "algorithm", "programming", "computer", "software", "database",
    "function", "variable", "interface", "abstract", "collection",
    "inheritance", "polymorphism", "encapsulation", "framework", "library",
    "architecture", "development", "application", "system", "network",
    "security", "authentication", "encryption", "protocol", "server",
    "client", "backend", "frontend", "fullstack", "debugging",
    "testing", "deployment", "version", "repository", "branch",
    "commit", "merge", "conflict", "refactor", "optimize",
    "performance", "scalability", "availability", "reliability", "maintainability",
    "documentation", "specification", "requirement", "design", "implementation",
    "integration", "continuous", "pipeline", "automation", "monitoring",
    "logging", "exception", "handling", "validation", "sanitization",
    "injection", "vulnerability", "firewall", "malware", "phishing",
    "cryptocurrency", "blockchain", "machine", "learning", "artificial",
    "intelligence", "neural", "network", "dataset", "training",
    "prediction", "classification", "regression", "clustering", "supervised",
    "unsupervised", "reinforcement", "backpropagation", "gradient", "descent",
    "overfitting", "underfitting", "hyperparameter", "crossvalidation", "ensemble"
  )

  // Get a random word from the list
  def getRandomWord(): String = {
    val random = new scala.util.Random()
    words(random.nextInt(words.length))
  }

  // Get a word based on difficulty (optional enhancement)
  def getWordByDifficulty(difficulty: String): String = {
    difficulty.toLowerCase match {
      case "easy" => words.filter(_.length <= 8)(scala.util.Random.nextInt(words.count(_.length <= 8)))
      case "medium" => words.filter(w => w.length > 8 && w.length <= 12)(scala.util.Random.nextInt(words.count(w => w.length > 8 && w.length <= 12)))
      case "hard" => words.filter(_.length > 12)(scala.util.Random.nextInt(words.count(_.length > 12)))
      case _ => getRandomWord()
    }
  }

  // Validate if a word is acceptable (> 5 letters)
  def isValidWord(word: String): Boolean = {
    word.length > 5 && word.forall(_.isLetter)
  }

  // Get total number of words in the list
  def wordCount: Int = words.length
}