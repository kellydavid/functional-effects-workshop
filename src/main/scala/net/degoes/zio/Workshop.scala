package net.degoes.zio

import zio._

import scala.annotation.tailrec

object HelloWorld extends App {
  import zio.console._

  /**
    * EXERCISE 1
    *
    * Implement a simple "Hello World" program using the effect returned by `putStrLn`.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn("Hello World!") *> ZIO.succeed(0)

//  val runtime = new DefaultRuntime {}
//  runtime.unsafeRun(putStrLn("Hello World"))
}

object ErrorConversion extends App {
  val StdInputFailed = 1

  import zio.console._

  val failed =
    putStrLn("About to fail...") *>
      ZIO.fail("Uh oh!") *>
      putStrLn("This will NEVER be printed!")

  /**
    * EXERCISE 2
    *
    * Using `ZIO#orElse` or `ZIO#fold`, have the `run` function compose the
    * preceding `failed` effect into the effect that `run` returns.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] = failed.fold(_ => StdInputFailed, _ => 0)
}

object PromptName extends App {
  val StdInputFailed = 1

  import zio.console._

  /**
    * EXERCISE 3
    *
    * Implement a simple program that asks the user for their name (using
    * `getStrLn`), and then prints it out to the user (using `putStrLn`).
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _     <- putStrLn("Please enter your name.")
      name  <- getStrLn
      _     <- putStrLn(s"Hello, $name!")
    } yield ()).fold(_ => StdInputFailed, _ => 0)
}

object ZIOTypes {
  type ??? = Nothing

  /**
    * EXERCISE 4
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A] = ZIO[Any, Throwable, A]
  type UIO[+A] = ZIO[Any, Nothing, A]
  type RIO[-R, +A] = ZIO[R, Throwable, A]
  type IO[+E, +A] = ZIO[Any, E, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object NumberGuesser extends App {
  import zio.console._
  import zio.random._

  def analyzeAnswer(random: Int, guess: String) =
    if (random.toString == guess.trim) putStrLn("You guessed correctly!")
    else putStrLn(s"You did not guess correctly. The answer was ${random}")

  /**
    * EXERCISE 5
    *
    * Choose a random number (using `nextInt`), and then ask the user to guess
    * the number, feeding their response to `analyzeAnswer`, above.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      randomNumber  <- nextInt(10)
      _             <- putStrLn("Guess a number up to 10.")
      guess         <- getStrLn
      _             <- analyzeAnswer(randomNumber, guess)
    } yield ()).fold(_ => 1, _ => 0)
}

object AlarmApp extends App {
  import zio.console._
  import zio.duration._
  import java.io.IOException

  /**
    * EXERCISE 6
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a decimal number of seconds.
    */
  lazy val getAlarmDuration: ZIO[Console, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      for {
        decimal   <- IO(Integer.parseInt(input)).refineToOrDie[NumberFormatException]
        duration  <- UIO(Duration(decimal, java.util.concurrent.TimeUnit.SECONDS))
      } yield duration


    def fallback(input: String): ZIO[Console, IOException, Duration] =
      for {
        _             <- putStrLn(input)
        durationStr   <- getStrLn
        duration      <- parseDuration(durationStr).orElse(fallback("You did not enter a decimal number."))
      } yield duration

    fallback("Please enter a decimal number of seconds.")
  }

  /**
    * EXERCISE 7
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds, and then prints out a wakeup
    * alarm message.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _           <- putStrLn("Select the length of time to sleep")
      duration    <- getAlarmDuration
      _           <- ZIO.sleep(duration)
      _           <- putStrLn("WAKEUP!")
    } yield ()).fold(_ => 1, _ => 0)
}

object Cat extends App {
  import zio.console._
  import zio.blocking._
  import java.io.IOException

  /**
    * EXERCISE 8
    *
    * Implement a function to read a file on the blocking thread pool, storing
    * the result into a string.
    */
  def readFile(file: String): ZIO[Blocking, IOException, String] =
    blocking {
      ZIO.effect(scala.io.Source.fromFile(new java.io.File(file)).mkString).refineToOrDie[IOException]
    }

  /**
    * EXERCISE 9
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      filename  <- Task(args.head)
      cat       <- readFile(filename)
      _         <- putStrLn(cat)
    } yield ()).fold(_ => 1, _ => 0)
}

object CatIncremental extends App {
  import zio.console._
  import zio.blocking._
  import java.io._

  /**
    * EXERCISE 10
    *
    * Implement all missing methods of `FileHandle`. Be sure to do all work on
    * the blocking thread pool.
    */
  final case class FileHandle private (private val is: InputStream) {
    final def close: ZIO[Blocking, IOException, Unit] =
      blocking {
        ZIO.effect(is.close()).refineToOrDie[IOException]
      }

    final def read: ZIO[Blocking, IOException, Option[Chunk[Byte]]] =
      blocking {
        ZIO.effect{
          val arr: Array[Byte] = Array.ofDim[Byte](1024)
          val readResult = is.read(arr)
          if (readResult == -1) None else Some(Chunk.fromArray(arr))
        }.refineToOrDie[IOException]
      }
  }
  object FileHandle {
    final def open(file: String): ZIO[Blocking, IOException, FileHandle] =
      blocking {
        ZIO.effect(new FileHandle(new FileInputStream(file))).refineToOrDie[IOException]
      }
  }

  /**
    * EXERCISE 11
    *
    * Implement an incremental version of the `cat` utility, using `ZIO#bracket`
    * or `ZManaged` to ensure the file is closed in the event of error or
    * interruption.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
  (args match {
    case Nil =>
      putStrLn("Please enter a file name.")
    case fileName :: _ =>
      FileHandle.open(fileName).bracket(_.close.orDie){fh =>
        lazy val printLoop: ZIO[Any with Blocking with zio.console.Console, IOException, Unit] =
          fh.read.flatMap{
            case Some(chunk: Chunk[Byte]) =>
              putStrLn(new String(chunk.toArray, java.nio.charset.StandardCharsets.UTF_8)) *>
              printLoop
            case None =>
              fh.close.orDie
          }
        printLoop
      }
  }).fold(_ => 1, _ => 0)

}

object ComputePi extends App {
  import zio.random._
  import zio.console._
  import zio.duration._

  /**
    * Some state to keep track of all points inside a circle,
    * and total number of points.
    */
  final case class PiState(
      inside: Ref[Long],
      total: Ref[Long]
  )

  /**
    * A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /**
    * A helper function that determines if a point lies in
    * a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /**
    * An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Random, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /*
  A fiber Fiber[E, A]
  can be thought of as a process that will run concurrently, not necessarily in parallel

  independent, logical threads of execution

  if you use 8 threads, then zio will allocate to all of them ie. 8 fibers in parallel

  use .fork to not run on main fork

  join will return result when fully evaluated
  can do a poll to check if done

  Fabio Cats Ref and Defer talk is very good

  STM is preferred

  DOn't use fiber, only for small pieces, use higher level constructs

   */

  /**
    * EXERCISE 12
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    for {
      // if interrupted before beginning, ensuring will not be executed (small race condition)
      fiber <- ((ZIO.sleep(10.seconds) *> putStrLn("Goodbye!")) ensuring putStrLn("Exiting...")).fork
      _     <- putStrLn("Hello!")
      _     <- fiber.interrupt
    } yield 0

}

object Hangman extends App {
  import zio.console._
  import zio.random._
  import java.io.IOException

  /**
    * EXERCISE 13
    *
    * Implement an effect that gets a single, lower-case character from
    * the user.
    */
  lazy val getChoice: ZIO[Console, IOException, Char] =
    getStrLn.flatMap{
      case str if str.length == 1 => UIO(str.charAt(0))
      case _                      => putStrLn("Please enter a single character.") *>
                                      getChoice
    }

  /**
    * EXERCISE 14
    *
    * Implement an effect that prompts the user for their name, and
    * returns it.
    */
  lazy val getName: ZIO[Console, IOException, String] =
    for {
      _     <- putStrLn("Please enter your name.")
      name  <- getStrLn
    } yield name

  /**
    * EXERCISE 15
    *
    * Implement an effect that chooses a random word from the dictionary.
    */
  lazy val chooseWord: ZIO[Random, Nothing, String] =
    for {
      randomNum   <- nextInt(Dictionary.Dictionary.size)
      word        <- UIO(Dictionary.Dictionary(randomNum))
    } yield word

  /**
    * EXERCISE 17
    *
    * Implement the main game loop, which gets choices from the user until
    * the game is won or lost.
    */
  def gameLoop(ref: Ref[State]): ZIO[Console, IOException, Unit] =
    for {
      oldState  <- ref.get
      _         <- renderState(oldState)
      choice    <- getChoice
      newState  <- ref.update(s => s.copy(guesses = s.guesses ++ Set(choice)))
      newRef    <- Ref.make(newState)
      result    <- UIO(guessResult(oldState, newState, choice))
      _         <- result match {
                      case GuessResult.Won =>
                        renderState(newState) *>
                          putStrLn("Congratulations, you won!")
                      case GuessResult.Correct =>
                        putStrLn("Correct guess!") *>
                          gameLoop(newRef)
                      case GuessResult.Incorrect =>
                        putStrLn("Incorrect guess :(") *>
                          gameLoop(newRef)
                      case GuessResult.Unchanged =>
                        putStrLn("You already tried that character.") *>
                          gameLoop(newRef)
                      case GuessResult.Lost =>
                        renderState(newState) *>
                          putStrLn("GAME OVER!") *>
                          putStrLn(s"The word was ${oldState.word}")
                    }
    } yield ()

  def renderState(state: State): ZIO[Console, Nothing, Unit] = {

    /**
      *
      *  f     n  c  t  o
      *  -  -  -  -  -  -  -
      *
      *  Guesses: a, z, y, x
      *
      */
    val word =
      state.word.toList
        .map(c => if (state.guesses.contains(c)) s" $c " else "   ")
        .mkString("")

    val line = List.fill(state.word.length)(" - ").mkString("")

    val guesses = " Guesses: " + state.guesses.mkString(", ")

    val text = word + "\n" + line + "\n\n" + guesses + "\n"

    putStrLn(text)
  }

  final case class State(name: String, guesses: Set[Char], word: String) {
    final def failures: Int = (guesses -- word.toSet).size

    final def playerLost: Boolean = failures > 10

    final def playerWon: Boolean = (word.toSet -- guesses).isEmpty

    final def addChar(char: Char): State = copy(guesses = guesses + char)
  }

  sealed trait GuessResult
  object GuessResult {
    case object Won extends GuessResult
    case object Lost extends GuessResult
    case object Correct extends GuessResult
    case object Incorrect extends GuessResult
    case object Unchanged extends GuessResult
  }

  def guessResult(oldState: State, newState: State, char: Char): GuessResult =
    if (oldState.guesses.contains(char)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(char)) GuessResult.Correct
    else GuessResult.Incorrect

  /**
    * EXERCISE 18
    *
    * Implement hangman using `Dictionary.Dictionary` for the words,
    * and the above helper functions.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    (for {
      _           <- putStrLn("Welcome to Hangman.")
      name        <- getName
      word        <- chooseWord
      firstState  <- Ref.make(State(name, Set.empty, word))
      _           <- gameLoop(firstState)
    } yield ()).fold(_ => 1, _ => 0)

}

/**
  * GRADUATION PROJECT
  *
  * Implement a game of tic tac toe using ZIO, then develop unit tests to
  * demonstrate its correctness and testability.
  */
object TicTacToe extends App {
  import zio.console._

  sealed trait Mark {
    final def renderChar: Char = this match {
      case Mark.X => 'X'
      case Mark.O => 'O'
    }
    final def render: String = renderChar.toString
  }
  object Mark {
    case object X extends Mark
    case object O extends Mark
  }

  final case class Board private (value: Vector[Vector[Option[Mark]]]) {

    /**
      * Retrieves the mark at the specified row/col.
      */
    final def get(row: Int, col: Int): Option[Mark] =
      value.lift(row).flatMap(_.lift(col)).flatten

    /**
      * Places a mark on the board at the specified row/col.
      */
    final def place(row: Int, col: Int, mark: Mark): Option[Board] =
      if (row >= 0 && col >= 0 && row < 3 && col < 3)
        Some(
          copy(value = value.updated(row, value(row).updated(col, Some(mark))))
        )
      else None

    /**
      * Renders the board to a string.
      */
    def render: String =
      value
        .map(_.map(_.fold(" ")(_.render)).mkString(" ", " | ", " "))
        .mkString("\n---|---|---\n")

    /**
      * Returns which mark won the game, if any.
      */
    final def won: Option[Mark] =
      if (wonBy(Mark.X)) Some(Mark.X)
      else if (wonBy(Mark.O)) Some(Mark.O)
      else None

    private final def wonBy(mark: Mark): Boolean =
      wonBy(0, 0, 1, 1, mark) ||
        wonBy(0, 2, 1, -1, mark) ||
        wonBy(0, 0, 0, 1, mark) ||
        wonBy(1, 0, 0, 1, mark) ||
        wonBy(2, 0, 0, 1, mark) ||
        wonBy(0, 0, 1, 0, mark) ||
        wonBy(0, 1, 1, 0, mark) ||
        wonBy(0, 2, 1, 0, mark)

    private final def wonBy(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int,
        mark: Mark
    ): Boolean =
      extractLine(row0, col0, rowInc, colInc).collect { case Some(v) => v }.toList == List
        .fill(3)(mark)

    private final def extractLine(
        row0: Int,
        col0: Int,
        rowInc: Int,
        colInc: Int
    ): Iterable[Option[Mark]] =
      for {
        row <- (row0 to (row0 + rowInc * 2))
        col <- (col0 to (col0 + colInc * 2))
      } yield value(row)(col)
  }
  object Board {
    final val empty = new Board(Vector.fill(3)(Vector.fill(3)(None)))

    def fromChars(
        first: Iterable[Char],
        second: Iterable[Char],
        third: Iterable[Char]
    ): Option[Board] =
      if (first.size != 3 || second.size != 3 || third.size != 3) None
      else {
        def toMark(char: Char): Option[Mark] =
          if (char.toLower == 'x') Some(Mark.X)
          else if (char.toLower == 'o') Some(Mark.O)
          else None

        Some(
          new Board(
            Vector(
              first.map(toMark).toVector,
              second.map(toMark).toVector,
              third.map(toMark).toVector
            )
          )
        )
      }
  }

  val TestBoard = Board
    .fromChars(
      List(' ', 'O', 'X'),
      List('O', 'X', 'O'),
      List('X', ' ', ' ')
    )
    .get
    .render

  /**
    * The entry point to the game will be here.
    */
  def run(args: List[String]): ZIO[ZEnv, Nothing, Int] =
    putStrLn(TestBoard) as 0
}
