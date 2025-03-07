"use client"

import { useState, useEffect } from "react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { AlertCircle, CheckCircle2 } from "lucide-react"
import { softwareTerms } from "@/data/software-terms"

type SoftwareTerm = {
  name: string
  releaseYear: number
  primaryPurpose: string
  category1: string
  category2: string
  category3: string
}

type Guess = {
  term: string
  feedback: {
    releaseYear: "correct" | "related" | "incorrect"
    primaryPurpose: "correct" | "related" | "incorrect"
    category1: "correct" | "related" | "incorrect"
    category2: "correct" | "related" | "incorrect"
    category3: "correct" | "related" | "incorrect"
  }
}

export default function SoftwareWordle() {
  const [targetTerm, setTargetTerm] = useState<SoftwareTerm | null>(null)
  const [guesses, setGuesses] = useState<Guess[]>([])
  const [currentGuess, setCurrentGuess] = useState("")
  const [gameStatus, setGameStatus] = useState<"playing" | "won" | "lost">("playing")
  const [error, setError] = useState<string | null>(null)
  const maxAttempts = 6

  useEffect(() => {
    // Select a random software term when the component mounts
    const randomIndex = Math.floor(Math.random() * softwareTerms.length)
    setTargetTerm(softwareTerms[1])
  }, [])

  const handleGuess = () => {
    if (!targetTerm) return
    if (currentGuess.trim() === "") {
      setError("Please enter a guess")
      return
    }

    // Find the guessed term in our database
    const guessedTerm = softwareTerms.find((term) => term.name.toLowerCase() === currentGuess.toLowerCase())

    if (!guessedTerm) {
      setError("Term not found in our database")
      return
    }

    setError(null)

    // Calculate feedback
    const feedback = {
      releaseYear: calculateYearFeedback(guessedTerm.releaseYear, targetTerm.releaseYear),
      primaryPurpose: calculateStringFeedback(guessedTerm.primaryPurpose, targetTerm.primaryPurpose),
      category1: calculateStringFeedback(guessedTerm.category1, targetTerm.category1),
      category2: calculateStringFeedback(guessedTerm.category2, targetTerm.category2),
      category3: calculateStringFeedback(guessedTerm.category3, targetTerm.category3),
    }

    const newGuess: Guess = {
      term: guessedTerm.name,
      feedback,
    }

    const updatedGuesses = [...guesses, newGuess]
    setGuesses(updatedGuesses)
    setCurrentGuess("")

    // Check if the player won
    if (guessedTerm.name.toLowerCase() === targetTerm.name.toLowerCase()) {
      setGameStatus("won")
    } else if (updatedGuesses.length >= maxAttempts) {
      setGameStatus("lost")
    }
  }

  const calculateYearFeedback = (guessYear: number, targetYear: number): "correct" | "related" | "incorrect" => {
    if (guessYear === targetYear) return "correct"
    if (Math.abs(guessYear - targetYear) <= 3) return "related"
    return "incorrect"
  }

  const calculateStringFeedback = (guessValue: string, targetValue: string): "correct" | "related" | "incorrect" => {
    if (guessValue.toLowerCase() === targetValue.toLowerCase()) return "correct"

    // Check if there are common words between the two strings
    const guessWords = guessValue.toLowerCase().split(/\s+/)
    const targetWords = targetValue.toLowerCase().split(/\s+/)

    for (const word of guessWords) {
      if (word.length > 2 && targetWords.includes(word)) return "related"
    }

    // Check if they're related based on common substrings
    if (
      guessValue.toLowerCase().includes(targetValue.toLowerCase()) ||
      targetValue.toLowerCase().includes(guessValue.toLowerCase())
    ) {
      return "related"
    }

    return "incorrect"
  }

  const resetGame = () => {
    const randomIndex = Math.floor(Math.random() * softwareTerms.length)
    setTargetTerm(softwareTerms[randomIndex])
    setGuesses([])
    setCurrentGuess("")
    setGameStatus("playing")
    setError(null)
  }

  const getFeedbackColor = (feedback: "correct" | "related" | "incorrect") => {
    switch (feedback) {
      case "correct":
        return "bg-green-500"
      case "related":
        return "bg-yellow-500"
      case "incorrect":
        return "bg-gray-500"
    }
  }

  return (
    <div className="w-full max-w-3xl mx-auto">
      {gameStatus === "playing" && (
        <div className="mb-6">
          <div className="flex gap-2">
            <Input
              type="text"
              value={currentGuess}
              onChange={(e) => setCurrentGuess(e.target.value)}
              placeholder="Enter a software term"
              className="flex-1"
              onKeyDown={(e) => {
                if (e.key === "Enter") handleGuess()
              }}
            />
            <Button onClick={handleGuess}>Guess</Button>
          </div>
          {error && <p className="mt-2 text-sm text-red-500">{error}</p>}
        </div>
      )}

      {gameStatus === "won" && (
        <Alert className="mb-6 bg-green-50 border-green-200">
          <CheckCircle2 className="h-4 w-4 text-green-600" />
          <AlertTitle className="text-green-800">Congratulations!</AlertTitle>
          <AlertDescription className="text-green-700">
            You guessed the correct term: <strong>{targetTerm?.name}</strong>
          </AlertDescription>
          <Button onClick={resetGame} className="mt-2 bg-green-600 hover:bg-green-700">
            Play Again
          </Button>
        </Alert>
      )}

      {gameStatus === "lost" && (
        <Alert className="mb-6 bg-red-50 border-red-200">
          <AlertCircle className="h-4 w-4 text-red-600" />
          <AlertTitle className="text-red-800">Game Over</AlertTitle>
          <AlertDescription className="text-red-700">
            The correct term was: <strong>{targetTerm?.name}</strong>
          </AlertDescription>
          <Button onClick={resetGame} className="mt-2 bg-red-600 hover:bg-red-700">
            Play Again
          </Button>
        </Alert>
      )}

      <div className="space-y-4">
        {targetTerm && gameStatus !== "playing" && (
          <div className="p-4 border rounded-lg bg-gray-50">
            <h3 className="font-bold text-lg mb-2">Correct Answer: {targetTerm.name}</h3>
            <div className="grid grid-cols-2 gap-2">
              <div className="font-semibold">Release Year:</div>
              <div>{targetTerm.releaseYear}</div>
              <div className="font-semibold">Primary Purpose:</div>
              <div>{targetTerm.primaryPurpose}</div>
              <div className="font-semibold">Category 1:</div>
              <div>{targetTerm.category1}</div>
              <div className="font-semibold">Category 2:</div>
              <div>{targetTerm.category2}</div>
              <div className="font-semibold">Category 3:</div>
              <div>{targetTerm.category3}</div>
            </div>
          </div>
        )}

        <div className="grid grid-cols-6 gap-2 font-semibold text-center mb-2">
          <div>Term</div>
          <div>Year</div>
          <div>Purpose</div>
          <div>Cat 1</div>
          <div>Cat 2</div>
          <div>Cat 3</div>
        </div>

        {guesses.map((guess, index) => (
          <div key={index} className="grid grid-cols-6 gap-2 text-center">
            <div className="font-medium">{guess.term}</div>
            <div className={`${getFeedbackColor(guess.feedback.releaseYear)} text-white rounded p-1`}>
              {guess.feedback.releaseYear === "correct" ? "✓" : guess.feedback.releaseYear === "related" ? "≈" : "✗"}
            </div>
            <div className={`${getFeedbackColor(guess.feedback.primaryPurpose)} text-white rounded p-1`}>
              {guess.feedback.primaryPurpose === "correct"
                ? "✓"
                : guess.feedback.primaryPurpose === "related"
                  ? "≈"
                  : "✗"}
            </div>
            <div className={`${getFeedbackColor(guess.feedback.category1)} text-white rounded p-1`}>
              {guess.feedback.category1 === "correct" ? "✓" : guess.feedback.category1 === "related" ? "≈" : "✗"}
            </div>
            <div className={`${getFeedbackColor(guess.feedback.category2)} text-white rounded p-1`}>
              {guess.feedback.category2 === "correct" ? "✓" : guess.feedback.category2 === "related" ? "≈" : "✗"}
            </div>
            <div className={`${getFeedbackColor(guess.feedback.category3)} text-white rounded p-1`}>
              {guess.feedback.category3 === "correct" ? "✓" : guess.feedback.category3 === "related" ? "≈" : "✗"}
            </div>
          </div>
        ))}

        {guesses.length === 0 && (
          <div className="text-center p-4 border rounded-lg bg-gray-50">Make your first guess to start the game!</div>
        )}
      </div>

      <div className="mt-6">
        <h3 className="font-bold mb-2">How to Play:</h3>
        <ul className="list-disc pl-5 space-y-1">
          <li>Guess a software term (programming language, framework, library, tool)</li>
          <li>Get feedback on how close your guess is to the target</li>
          <li>
            <span className="inline-block w-4 h-4 bg-green-500 rounded-full"></span> Green: Exact match
          </li>
          <li>
            <span className="inline-block w-4 h-4 bg-yellow-500 rounded-full"></span> Yellow: Related but not exact
          </li>
          <li>
            <span className="inline-block w-4 h-4 bg-gray-500 rounded-full"></span> Gray: Not related
          </li>
          <li>You have {maxAttempts} attempts to guess correctly</li>
        </ul>
      </div>
    </div>
  )
}

