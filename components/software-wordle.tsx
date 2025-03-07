"use client"

import { useState, useEffect, useMemo } from "react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@/components/ui/tooltip"
import { AlertCircle, CheckCircle2, ArrowUp, ArrowDown } from "lucide-react"
import { SoftwareTerm, softwareTerms } from "@/data/software-terms"

type Guess = {
  term: string
  termData: SoftwareTerm
  feedback: {
    releaseYear: "correct" | "related" | "incorrect"
    yearDirection?: "higher" | "lower"
    type: "correct" | "related" | "incorrect"
    paradigm: "correct" | "related" | "incorrect"
    domain: "correct" | "related" | "incorrect"
    company: "correct" | "related" | "incorrect"
  }
}

export default function SoftwareWordle() {
  const [targetTerm, setTargetTerm] = useState<SoftwareTerm | null>(null)
  const [guesses, setGuesses] = useState<Guess[]>([])
  const [currentGuess, setCurrentGuess] = useState("")
  const [gameStatus, setGameStatus] = useState<"playing" | "won" | "lost">("playing")
  const [error, setError] = useState<string | null>(null)
  const [showSuggestions, setShowSuggestions] = useState(false);
  const [selectedIndex, setSelectedIndex] = useState(-1);
  const [suggestions, setSuggestions] = useState<string[]>([])
  const maxAttempts = 6

  const codeSnippetParts = useMemo(() => {
    if (!targetTerm) return [];
    return targetTerm.codeSnippet.split("\n");
  }, [targetTerm]);

  useEffect(() => {
    const randomIndex = Math.floor(Math.random() * softwareTerms.length)
    setTargetTerm(softwareTerms[randomIndex])
  }, [])

  useEffect(() => {
    if (currentGuess.trim() === "") {
      setSuggestions([])
      return
    }
    const filteredSuggestions = softwareTerms
      .map(term => term.name)
      .filter(name => name.toLowerCase().includes(currentGuess.toLowerCase()))
    setSuggestions(filteredSuggestions)
  }, [currentGuess])

  useEffect(() => {
    if (suggestions.length > 0) {
      setSelectedIndex(0); 
    } else {
      setSelectedIndex(-1); 
    }
  }, [suggestions]);

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "ArrowDown" && showSuggestions) {
      e.preventDefault();
      setSelectedIndex(prev => (prev < suggestions.length - 1 ? prev + 1 : prev));
    } else if (e.key === "ArrowUp" && showSuggestions) {
      e.preventDefault();
      setSelectedIndex(prev => (prev > 0 ? prev - 1 : 0));
    } else if (e.key === "Enter") {
      if (showSuggestions && selectedIndex >= 0) {
        setCurrentGuess(suggestions[selectedIndex]);
        setShowSuggestions(false);
      } else {
        handleGuess();
      }
    } else if (e.key === "Escape") {
      setShowSuggestions(false);
    }
  };

  const handleGuess = () => {
    if (!targetTerm) return
    if (currentGuess.trim() === "") {
      setError("Please enter a guess")
      return
    }

    if (guesses.some(guess => guess.term.toLowerCase() === currentGuess.toLowerCase())) {
      setError("You've already guessed that term");
      return;
    }

    const guessedTerm = softwareTerms.find((term) => term.name.toLowerCase() === currentGuess.toLowerCase())
    if (!guessedTerm) {
      setError("Term not found in our database")
      return
    }

    setError(null)

    const yearFeedback = calculateYearFeedback(guessedTerm.releaseYear, targetTerm.releaseYear)
    const feedback = {
      releaseYear: yearFeedback.status,
      yearDirection: yearFeedback.direction,
      type: calculateStringFeedback(guessedTerm.type, targetTerm.type),
      paradigm: calculateStringFeedback(guessedTerm.paradigm, targetTerm.paradigm),
      domain: calculateStringFeedback(guessedTerm.domain, targetTerm.domain),
      company: calculateStringFeedback(guessedTerm.company, targetTerm.company),
    }

    const newGuess: Guess = {
      term: guessedTerm.name,
      termData: guessedTerm,
      feedback,
    }

    const updatedGuesses = [...guesses, newGuess]
    setGuesses(updatedGuesses)
    setCurrentGuess("")

    if (guessedTerm.name.toLowerCase() === targetTerm.name.toLowerCase()) {
      setGameStatus("won")
    } else if (updatedGuesses.length >= maxAttempts) {
      setGameStatus("lost")
    }
  }

  const revealedCodeParts = guesses.length > 0 ? codeSnippetParts.slice(0, Math.min(guesses.length, 5)) : [];

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
    if (guessValue.toLowerCase() === targetValue.toLowerCase()) return "correct"
    const normalizedGuess = guessValue.toLowerCase().trim()
    const normalizedTarget = targetValue.toLowerCase().trim()
    if (normalizedGuess.includes(normalizedTarget) || normalizedTarget.includes(normalizedGuess)) {
      return "related"
    }
    const guessWords = normalizedGuess.split(/[\s-.,;:\/]+/).filter(word => word.length > 1)
    const targetWords = normalizedTarget.split(/[\s-.,;:\/]+/).filter(word => word.length > 1)
    const matchingWords = guessWords.filter(word => targetWords.includes(word))
    if (matchingWords.length > 0) return "related"
    if (levenshteinDistance(normalizedGuess, normalizedTarget) <= 3) return "related"
    return "incorrect"
  }

  function levenshteinDistance(str1: string, str2: string): number {
    const track = Array(str2.length + 1).fill(null).map(() => Array(str1.length + 1).fill(null))
    for (let i = 0; i <= str1.length; i += 1) track[0][i] = i
    for (let j = 0; j <= str2.length; j += 1) track[j][0] = j
    for (let j = 1; j <= str2.length; j += 1) {
      for (let i = 1; i <= str1.length; i += 1) {
        const indicator = str1[i - 1] === str2[j - 1] ? 0 : 1
        track[j][i] = Math.min(
          track[j][i - 1] + 1,
          track[j - 1][i] + 1,
          track[j - 1][i - 1] + indicator,
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
  }

  const getFeedbackColor = (feedback: "correct" | "related" | "incorrect") => {
    switch (feedback) {
      case "correct": return "bg-green-500"
      case "related": return "bg-yellow-500"
      case "incorrect": return "bg-gray-500"
    }
  }

  const getCategoryDescription = (category: string): string => {
    switch (category) {
      case "type": return "The classification of the technology (Programming Language, Framework, Library, Tool, etc.)"
      case "paradigm": return "The programming approach or methodology associated with the technology"
      case "domain": return "The primary field or area where the technology is applied"
      case "company": return "The organization, foundation, or entity that created or maintains the technology"
      default: return ""
    }
  }

  return (
    <div className="w-full max-w-4xl mx-auto p-4">
      {gameStatus === "playing" && (
        <div className="mb-6">
          <div className="flex gap-2 mb-2">
            <div className="relative flex-1">
              <Input
                type="text"
                value={currentGuess}
                onChange={(e) => {
                  setCurrentGuess(e.target.value);
                  setShowSuggestions(true);
                }}
                onFocus={() => setShowSuggestions(true)}
                onBlur={() => setTimeout(() => setShowSuggestions(false), 100)}
                onKeyDown={handleKeyDown}
                placeholder="Enter a language or framework"
                className="w-full p-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-zinc-500"
              />
              {showSuggestions && suggestions.length > 0 && (
                <div className="absolute z-10 w-full mt-1 bg-white border border-gray-300 rounded-md shadow-lg max-h-60 overflow-auto transition-all duration-200 ease-in-out">
                  {suggestions.map((suggestion, index) => (
                    <div
                      key={index}
                      className={`p-2 cursor-pointer hover:bg-zinc-50 ${index === selectedIndex ? "bg-zinc-100" : ""
                        }`}
                      onMouseDown={() => {
                        setCurrentGuess(suggestion);
                        setShowSuggestions(false);
                      }}
                    >
                      {suggestion}
                    </div>
                  ))}
                </div>
              )}
            </div>
            <Button
              onClick={handleGuess}
              className="bg-zinc-700 text-white px-4 py-2 rounded-md"
            >
              Guess
            </Button>
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
              <div className="font-semibold">Type:</div>
              <div>{targetTerm.type}</div>
              <div className="font-semibold">Paradigm:</div>
              <div>{targetTerm.paradigm}</div>
              <div className="font-semibold">Domain:</div>
              <div>{targetTerm.domain}</div>
              <div className="font-semibold">Company:</div>
              <div>{targetTerm.company}</div>
            </div>
          </div>
        )}

        {gameStatus === "playing" && revealedCodeParts.length > 0 && (
          <div className="mb-6 p-4 bg-gray-100 rounded-lg border">
            <h3 className="font-bold mb-2">Code Hint:</h3>
            <pre className="text-sm font-mono">
              {revealedCodeParts.map((part, index) => (
                <div key={index}>{part}</div>
              ))}
            </pre>
          </div>
        )}

        <div className="relative overflow-x-auto rounded-lg border">
          <table className="w-full">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-4 py-2 text-left">Term</th>
                <th className="px-3 py-2 text-center">
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger className="cursor-help">Year</TooltipTrigger>
                      <TooltipContent>
                        <p>The year when the technology was first released</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </th>
                <th className="px-3 py-2 text-center">
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger className="cursor-help">Type</TooltipTrigger>
                      <TooltipContent>
                        <p>{getCategoryDescription("type")}</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </th>
                <th className="px-3 py-2 text-center">
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger className="cursor-help">Paradigm</TooltipTrigger>
                      <TooltipContent>
                        <p>{getCategoryDescription("paradigm")}</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </th>
                <th className="px-3 py-2 text-center">
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger className="cursor-help">Domain</TooltipTrigger>
                      <TooltipContent>
                        <p>{getCategoryDescription("domain")}</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </th>
                <th className="px-3 py-2 text-center">
                  <TooltipProvider>
                    <Tooltip>
                      <TooltipTrigger className="cursor-help">Company</TooltipTrigger>
                      <TooltipContent>
                        <p>{getCategoryDescription("company")}</p>
                      </TooltipContent>
                    </Tooltip>
                  </TooltipProvider>
                </th>
              </tr>
            </thead>
            <tbody>
              {guesses.slice().reverse().map((guess, index) => (
                <tr key={index} className="border-t">
                  <td className="px-4 py-3">
                    <div className="font-medium">{guess.term}</div>
                  </td>
                  <td className="px-1 py-3">
                    <TooltipProvider>
                      <Tooltip>
                        <TooltipTrigger asChild>
                          <div className={`flex items-center justify-center ${getFeedbackColor(guess.feedback.releaseYear)} text-white rounded-md p-2`}>
                            {guess.termData.releaseYear}
                            {guess.feedback.releaseYear !== "correct" && (
                              guess.feedback.yearDirection === "higher" ? (
                                <ArrowDown className="h-4 w-4 ml-1" />
                              ) : (
                                <ArrowUp className="h-4 w-4 ml-1" />
                              )
                            )}
                          </div>
                        </TooltipTrigger>
                        <TooltipContent>
                          {gameStatus !== "playing" && guess.feedback.releaseYear !== "correct" && (
                            <p>Correct year: {targetTerm?.releaseYear}</p>
                          )}
                        </TooltipContent>
                      </Tooltip>
                    </TooltipProvider>
                  </td>
                  {["type", "paradigm", "domain", "company"].map((category) => (
                    <td key={category} className="px-2 py-3">
                      <TooltipProvider>
                        <Tooltip>
                          <TooltipTrigger asChild>
                            <div className={`${getFeedbackColor(guess.feedback[category])} text-white rounded-md p-2 text-center`}>
                              {guess.termData[category]}
                            </div>
                          </TooltipTrigger>
                          <TooltipContent>
                            {gameStatus !== "playing" && guess.feedback[category] !== "correct" && (
                              <p>Correct answer: {targetTerm?.[category]}</p>
                            )}
                          </TooltipContent>
                        </Tooltip>
                      </TooltipProvider>
                    </td>
                  ))}
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
        <h3 className="font-bold mb-3">Categories Explained:</h3>
        <div className="space-y-3">
          <div>
            <span className="font-semibold">Type:</span> Classification of the technology (Programming Language, Framework, Library, etc.)
          </div>
          <div>
            <span className="font-semibold">Paradigm:</span> Programming approach or methodology (OOP, Functional, Multi-paradigm, etc.)
          </div>
          <div>
            <span className="font-semibold">Domain:</span> Primary field of application (Web, Mobile, Data Science, DevOps, etc.)
          </div>
          <div>
            <span className="font-semibold">Company:</span> Organization or entity behind the technology
          </div>
        </div>

        <h3 className="font-bold mt-4 mb-2">How to Play:</h3>
        <ul className="space-y-2">
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>Guess a software term (programming language, framework, library, tool)</span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For the <strong>Year</strong> column: The guessed year is shown with background color:
              <div className="flex items-center mt-1 gap-2">
                <span className="w-6 h-6 bg-green-500 rounded-md"></span> Exact match
                <span className="w-6 h-6 bg-yellow-500 rounded-md"></span> Within 3 years
                <span className="w-6 h-6 bg-gray-500 rounded-md"></span> More than 3 years away
              </div>
              An arrow (↑ or ↓) indicates if the target year is higher or lower when not correct.
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For other categories: The guessed value is shown with background color:
              <div className="flex items-center mt-1 gap-2">
                <span className="w-6 h-6 bg-green-500 rounded-md"></span> Exact match
                <span className="w-6 h-6 bg-yellow-500 rounded-md"></span> Related
                <span className="w-6 h-6 bg-gray-500 rounded-md"></span> Not related
              </div>
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>Hover over cells after the game ends to see the correct answers for incorrect guesses</span>
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