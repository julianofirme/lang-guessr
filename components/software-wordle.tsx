"use client"

import { useState, useEffect } from "react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Badge } from "@/components/ui/badge"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@/components/ui/tooltip"
import { AlertCircle, CheckCircle2, HelpCircle, ArrowUp, ArrowDown, Check } from "lucide-react"
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
  termData: SoftwareTerm
  feedback: {
    releaseYear: "correct" | "related" | "incorrect"
    yearDirection?: "higher" | "lower"
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
  const [showHints, setShowHints] = useState(false)
  const maxAttempts = 6

  useEffect(() => {
    // Select a random software term when the component mounts
    const randomIndex = Math.floor(Math.random() * softwareTerms.length)
    setTargetTerm(softwareTerms[randomIndex])
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
    const yearFeedback = calculateYearFeedback(guessedTerm.releaseYear, targetTerm.releaseYear)
    const feedback = {
      releaseYear: yearFeedback.status,
      yearDirection: yearFeedback.direction,
      primaryPurpose: calculateStringFeedback(guessedTerm.primaryPurpose, targetTerm.primaryPurpose),
      category1: calculateStringFeedback(guessedTerm.category1, targetTerm.category1),
      category2: calculateStringFeedback(guessedTerm.category2, targetTerm.category2),
      category3: calculateStringFeedback(guessedTerm.category3, targetTerm.category3),
    }

    const newGuess: Guess = {
      term: guessedTerm.name,
      termData: guessedTerm,
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

  const calculateYearFeedback = (guessYear: number, targetYear: number): { status: "correct" | "related" | "incorrect", direction?: "higher" | "lower" } => {
    if (guessYear === targetYear) return { status: "correct" }
    if (Math.abs(guessYear - targetYear) <= 3) {
      return { 
        status: "related", 
        direction: guessYear > targetYear ? "higher" : "lower" 
      }
    }
    return { 
      status: "incorrect", 
      direction: guessYear > targetYear ? "higher" : "lower" 
    }
  }

  const calculateStringFeedback = (guessValue: string, targetValue: string): "correct" | "related" | "incorrect" => {
    // Exact match
    if (guessValue.toLowerCase() === targetValue.toLowerCase()) return "correct"
    
    // Normalize strings for comparison
    const normalizedGuess = guessValue.toLowerCase().trim()
    const normalizedTarget = targetValue.toLowerCase().trim()
    
    // Check for direct substring relationship
    if (normalizedGuess.includes(normalizedTarget) || 
        normalizedTarget.includes(normalizedGuess)) {
      return "related"
    }
    
    // Check for common words - improved to handle technical terms better
    const guessWords = normalizedGuess.split(/[\s-.,;:]+/).filter(word => word.length > 1)
    const targetWords = normalizedTarget.split(/[\s-.,;:]+/).filter(word => word.length > 1)
    
    // Count matching words
    const matchingWords = guessWords.filter(word => targetWords.includes(word))
    if (matchingWords.length > 0) {
      return "related"
    }
    
    // Check for Levenshtein distance for similar terms
    if (levenshteinDistance(normalizedGuess, normalizedTarget) <= 3) {
      return "related"
    }
    
    return "incorrect"
  }
  
  // Helper function for Levenshtein distance to find similar words
  function levenshteinDistance(str1: string, str2: string): number {
    const track = Array(str2.length + 1).fill(null).map(() => 
      Array(str1.length + 1).fill(null))
    
    for (let i = 0; i <= str1.length; i += 1) {
      track[0][i] = i
    }
    
    for (let j = 0; j <= str2.length; j += 1) {
      track[j][0] = j
    }
    
    for (let j = 1; j <= str2.length; j += 1) {
      for (let i = 1; i <= str1.length; i += 1) {
        const indicator = str1[i - 1] === str2[j - 1] ? 0 : 1
        track[j][i] = Math.min(
          track[j][i - 1] + 1, // deletion
          track[j - 1][i] + 1, // insertion
          track[j - 1][i - 1] + indicator, // substitution
        )
      }
    }
    
    return track[str2.length][str1.length]
  }

  const resetGame = () => {
    const randomIndex = Math.floor(Math.random() * softwareTerms.length)
    setTargetTerm(softwareTerms[randomIndex])
    setGuesses([])
    setCurrentGuess("")
    setGameStatus("playing")
    setError(null)
    setShowHints(false)
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

  const getAvailableTerms = () => {
    return softwareTerms.map(term => term.name).sort()
  }

  const toggleHints = () => {
    setShowHints(!showHints)
  }

  return (
    <div className="w-full max-w-3xl mx-auto p-4">
      <h1 className="text-2xl font-bold text-center mb-6">Software Terminology Wordle</h1>
      
      {gameStatus === "playing" && (
        <div className="mb-6">
          <div className="flex gap-2 mb-2">
            <Input
              type="text"
              value={currentGuess}
              onChange={(e) => setCurrentGuess(e.target.value)}
              placeholder="Enter a software term"
              className="flex-1"
              onKeyDown={(e) => {
                if (e.key === "Enter") handleGuess()
              }}
              list="term-suggestions"
            />
            <Button onClick={handleGuess}>Guess</Button>
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger asChild>
                  <Button variant="outline" size="icon" onClick={toggleHints}>
                    <HelpCircle className="h-4 w-4" />
                  </Button>
                </TooltipTrigger>
                <TooltipContent>
                  <p>Show available terms</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
          
          {showHints && (
            <div className="mt-2 p-3 bg-gray-50 rounded-md border max-h-40 overflow-y-auto">
              <p className="font-semibold mb-1">Available terms:</p>
              <div className="flex flex-wrap gap-1">
                {getAvailableTerms().map((term, index) => (
                  <Badge key={index} variant="outline" className="cursor-pointer" onClick={() => setCurrentGuess(term)}>
                    {term}
                  </Badge>
                ))}
              </div>
            </div>
          )}
          
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

        <div className="relative overflow-x-auto rounded-lg border">
          <table className="w-full">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-4 py-2 text-left">Term</th>
                <th className="px-3 py-2 text-center">Year</th>
                <th className="px-3 py-2 text-center">Purpose</th>
                <th className="px-3 py-2 text-center">Cat 1</th>
                <th className="px-3 py-2 text-center">Cat 2</th>
                <th className="px-3 py-2 text-center">Cat 3</th>
              </tr>
            </thead>
            <tbody>
              {guesses.map((guess, index) => (
                <tr key={index} className="border-t">
                  <td className="px-4 py-3">
                    <div className="font-medium">{guess.term}</div>
                    {gameStatus !== "playing" && (
                      <div className="text-xs text-gray-500 mt-1">{guess.termData.releaseYear}</div>
                    )}
                  </td>
                  <td className="px-2 py-3">
                    <div className={`flex items-center justify-center ${getFeedbackColor(guess.feedback.releaseYear)} text-white rounded-md p-2`}>
                      {guess.feedback.releaseYear === "correct" ? (
                        <Check className="h-4 w-4" />
                      ) : guess.feedback.yearDirection === "higher" ? (
                        <ArrowDown className="h-4 w-4" />
                      ) : (
                        <ArrowUp className="h-4 w-4" />
                      )}
                    </div>
                  </td>
                  <td className="px-2 py-3">
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <div className={`${getFeedbackColor(guess.feedback.primaryPurpose)} text-white rounded-md p-2 text-center cursor-help`}>
                            {guess.feedback.primaryPurpose === "correct" ? (
                              <Check className="h-4 w-4 mx-auto" />
                            ) : guess.feedback.primaryPurpose === "related" ? (
                              "≈"
                            ) : (
                              "✗"
                            )}
                          </div>
                        </TooltipTrigger>
                        <TooltipContent>
                          <p className="font-medium">Your guess:</p>
                          <p>{guess.termData.primaryPurpose}</p>
                          {guess.feedback.primaryPurpose !== "correct" && gameStatus !== "playing" && (
                            <>
                              <p className="font-medium mt-1">Correct answer:</p>
                              <p>{targetTerm?.primaryPurpose}</p>
                            </>
                          )}
                        </TooltipContent>
                      </Tooltip>
                    </TooltipProvider>
                  </td>
                  <td className="px-2 py-3">
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <div className={`${getFeedbackColor(guess.feedback.category1)} text-white rounded-md p-2 text-center cursor-help`}>
                            {guess.feedback.category1 === "correct" ? (
                              <Check className="h-4 w-4 mx-auto" />
                            ) : guess.feedback.category1 === "related" ? (
                              "≈"
                            ) : (
                              "✗"
                            )}
                          </div>
                        </TooltipTrigger>
                        <TooltipContent>
                          <p className="font-medium">Your guess:</p>
                          <p>{guess.termData.category1}</p>
                          {guess.feedback.category1 !== "correct" && gameStatus !== "playing" && (
                            <>
                              <p className="font-medium mt-1">Correct answer:</p>
                              <p>{targetTerm?.category1}</p>
                            </>
                          )}
                        </TooltipContent>
                      </Tooltip>
                    </TooltipProvider>
                  </td>
                  <td className="px-2 py-3">
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <div className={`${getFeedbackColor(guess.feedback.category2)} text-white rounded-md p-2 text-center cursor-help`}>
                            {guess.feedback.category2 === "correct" ? (
                              <Check className="h-4 w-4 mx-auto" />
                            ) : guess.feedback.category2 === "related" ? (
                              "≈"
                            ) : (
                              "✗"
                            )}
                          </div>
                        </TooltipTrigger>
                        <TooltipContent>
                          <p className="font-medium">Your guess:</p>
                          <p>{guess.termData.category2}</p>
                          {guess.feedback.category2 !== "correct" && gameStatus !== "playing" && (
                            <>
                              <p className="font-medium mt-1">Correct answer:</p>
                              <p>{targetTerm?.category2}</p>
                            </>
                          )}
                        </TooltipContent>
                      </Tooltip>
                    </TooltipProvider>
                  </td>
                  <td className="px-2 py-3">
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <div className={`${getFeedbackColor(guess.feedback.category3)} text-white rounded-md p-2 text-center cursor-help`}>
                            {guess.feedback.category3 === "correct" ? (
                              <Check className="h-4 w-4 mx-auto" />
                            ) : guess.feedback.category3 === "related" ? (
                              "≈"
                            ) : (
                              "✗"
                            )}
                          </div>
                        </TooltipTrigger>
                        <TooltipContent>
                          <p className="font-medium">Your guess:</p>
                          <p>{guess.termData.category3}</p>
                          {guess.feedback.category3 !== "correct" && gameStatus !== "playing" && (
                            <>
                              <p className="font-medium mt-1">Correct answer:</p>
                              <p>{targetTerm?.category3}</p>
                            </>
                          )}
                        </TooltipContent>
                      </Tooltip>
                    </TooltipProvider>
                  </td>
                </tr>
              ))}
              {guesses.length === 0 && (
                <tr>
                  <td colSpan={6} className="text-center p-4 text-gray-500">
                    Make your first guess to start the game!
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>

      <div className="mt-6 p-4 bg-gray-50 rounded-lg border">
        <h3 className="font-bold mb-2">How to Play:</h3>
        <ul className="space-y-2 pl-5">
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>Guess a software term (programming language, framework, library, tool)</span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For the <strong>Year</strong> column:
              <div className="flex items-center mt-1 gap-2">
                <span className="inline-block w-6 h-6 bg-green-500 rounded-md flex items-center justify-center text-white">
                  <Check className="h-4 w-4" />
                </span> Exact match
                <span className="inline-block w-6 h-6 bg-yellow-500 rounded-md flex items-center justify-center text-white">
                  <ArrowUp className="h-4 w-4" />
                </span> Target is newer
                <span className="inline-block w-6 h-6 bg-yellow-500 rounded-md flex items-center justify-center text-white">
                  <ArrowDown className="h-4 w-4" />
                </span> Target is older
              </div>
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For other categories:
              <div className="flex items-center mt-1 gap-2">
                <span className="inline-block w-6 h-6 bg-green-500 rounded-md flex items-center justify-center text-white">
                  <Check className="h-4 w-4" />
                </span> Exact match
                <span className="inline-block w-6 h-6 bg-yellow-500 rounded-md flex items-center justify-center text-white">≈</span> Related
                <span className="inline-block w-6 h-6 bg-gray-500 rounded-md flex items-center justify-center text-white">✗</span> Not related
              </div>
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>Hover over category cells to see your guess and (after the game) the correct answer</span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>You have {maxAttempts} attempts to guess correctly</span>
          </li>
        </ul>
      </div>
    </div>
  )
}