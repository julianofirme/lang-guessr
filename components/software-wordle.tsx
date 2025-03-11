"use client"

import { useState, useEffect, useMemo } from "react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert"
import { Tooltip, TooltipContent, TooltipProvider, TooltipTrigger } from "@/components/ui/tooltip"
import { AlertCircle, CheckCircle2, ArrowUp, ArrowDown, Share } from "lucide-react"
import { SoftwareTerm, softwareTerms } from "@/data/software-terms"
import toast from "react-hot-toast"

type FeedbackValue = "correct" | "related" | "incorrect";

type Feedback = {
  releaseYear: FeedbackValue;
  yearDirection?: "higher" | "lower";
  type: FeedbackValue;
  paradigm: FeedbackValue;
  domain: FeedbackValue;
  company: FeedbackValue;
};

type FeedbackKey = keyof Omit<Feedback, "yearDirection">;

type Guess = {
  term: string;
  termData: SoftwareTerm;
  feedback: Feedback;
};

export default function SoftwareWordle() {
  const [targetTerm, setTargetTerm] = useState<SoftwareTerm | null>(null)
  const [guesses, setGuesses] = useState<Guess[]>([])
  const [currentGuess, setCurrentGuess] = useState("")
  const [gameStatus, setGameStatus] = useState<"playing" | "won" | "lost">("playing")
  const [error, setError] = useState<string | null>(null)
  const [showSuggestions, setShowSuggestions] = useState(false)
  const [showHistoricalHint, setShowHistoricalHint] = useState(false)
  const [selectedIndex, setSelectedIndex] = useState(-1)
  const [suggestions, setSuggestions] = useState<string[]>([])
  const [dateString, setDateString] = useState("")
  const maxAttempts = 6

  const codeSnippetParts = useMemo(() => {
    if (!targetTerm) return [];
    return targetTerm.codeSnippet.split("\n");
  }, [targetTerm]);

  // Function to get the current UTC date string
  const getCurrentDateString = () => {
    const currentDate = new Date();
    const utcYear = currentDate.getUTCFullYear();
    const utcMonth = String(currentDate.getUTCMonth() + 1).padStart(2, '0');
    const utcDay = String(currentDate.getUTCDate()).padStart(2, '0');
    return `${utcYear}-${utcMonth}-${utcDay}`;
  }

  // Load daily term and game state on mount
  useEffect(() => {
    const currentDateString = getCurrentDateString();
    setDateString(currentDateString);

    // Calculate term index based on days since start date
    const startDate = new Date('2024-10-01T00:00:00Z').getTime();
    const currentDate = new Date(currentDateString + 'T00:00:00Z').getTime();
    const diffTime = currentDate - startDate;
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));
    const termIndex = (diffDays % softwareTerms.length + softwareTerms.length) % softwareTerms.length;
    setTargetTerm(softwareTerms[termIndex]);

    // Load game state from local storage
    const storageKey = `lang-guessr-${currentDateString}`;
    const savedState = localStorage.getItem(storageKey);
    if (savedState) {
      const { guesses, gameStatus } = JSON.parse(savedState);
      setGuesses(guesses);
      setGameStatus(gameStatus);
    } else {
      setGuesses([]);
      setGameStatus("playing");
    }
  }, []); 

  // Save game state to local storage when guesses or gameStatus changes
  useEffect(() => {
    if (dateString) {
      const storageKey = `lang-guessr-${dateString}`;
      localStorage.setItem(storageKey, JSON.stringify({ guesses, gameStatus }));
    }
  }, [guesses, gameStatus, dateString]);

  // Suggestion logic remains unchanged
  useEffect(() => {
    if (currentGuess.trim() === "") {
      setSuggestions([]);
      return;
    }
    const filteredSuggestions = softwareTerms
      .map(term => term.name)
      .filter(name => name.toLowerCase().includes(currentGuess.toLowerCase()));
    setSuggestions(filteredSuggestions);
  }, [currentGuess]);

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
    if (!targetTerm) return;
    if (currentGuess.trim() === "") {
      setError("Please enter a guess");
      return;
    }

    if (guesses.some(guess => guess.term.toLowerCase() === currentGuess.toLowerCase())) {
      setError("You've already guessed that term");
      return;
    }

    const guessedTerm = softwareTerms.find((term) => term.name.toLowerCase() === currentGuess.toLowerCase());
    if (!guessedTerm) {
      setError("Term not found in our database");
      return;
    }

    setError(null);

    const yearFeedback = calculateYearFeedback(guessedTerm.releaseYear, targetTerm.releaseYear);
    const feedback = {
      releaseYear: yearFeedback.status,
      yearDirection: yearFeedback.direction,
      type: calculateStringFeedback(guessedTerm.type, targetTerm.type),
      paradigm: calculateStringFeedback(guessedTerm.paradigm, targetTerm.paradigm),
      domain: calculateStringFeedback(guessedTerm.domain, targetTerm.domain),
      company: calculateStringFeedback(guessedTerm.company, targetTerm.company),
    };

    const newGuess: Guess = {
      term: guessedTerm.name,
      termData: guessedTerm,
      feedback,
    };

    const updatedGuesses = [...guesses, newGuess];
    setGuesses(updatedGuesses);
    setCurrentGuess("");

    if (guessedTerm.name.toLowerCase() === targetTerm.name.toLowerCase()) {
      setGameStatus("won");
    } else if (updatedGuesses.length >= maxAttempts) {
      setGameStatus("lost");
    }
  };

  const revealedCodeParts = guesses.length > 0 ? codeSnippetParts.slice(0, Math.min(guesses.length, 5)) : [];

  const calculateYearFeedback = (guessYear: number, targetYear: number): { status: FeedbackValue, direction?: "higher" | "lower" } => {
    if (guessYear === targetYear) return { status: "correct" };
    if (Math.abs(guessYear - targetYear) <= 3) {
      return {
        status: "related",
        direction: guessYear > targetYear ? "higher" : "lower"
      };
    }
    return {
      status: "incorrect",
      direction: guessYear > targetYear ? "higher" : "lower"
    };
  };

  const calculateStringFeedback = (guessValue: string, targetValue: string): FeedbackValue => {
    if (guessValue.toLowerCase() === targetValue.toLowerCase()) return "correct";
    const normalizedGuess = guessValue.toLowerCase().trim();
    const normalizedTarget = targetValue.toLowerCase().trim();
    if (normalizedGuess.includes(normalizedTarget) || normalizedTarget.includes(normalizedGuess)) {
      return "related";
    }
    const guessWords = normalizedGuess.split(/[\s-.,;:\/]+/).filter(word => word.length > 1);
    const targetWords = normalizedTarget.split(/[\s-.,;:\/]+/).filter(word => word.length > 1);
    const matchingWords = guessWords.filter(word => targetWords.includes(word));
    if (matchingWords.length > 0) return "related";
    if (levenshteinDistance(normalizedGuess, normalizedTarget) <= 3) return "related";
    return "incorrect";
  };

  function levenshteinDistance(str1: string, str2: string): number {
    const track = Array(str2.length + 1).fill(null).map(() => Array(str1.length + 1).fill(null));
    for (let i = 0; i <= str1.length; i += 1) track[0][i] = i;
    for (let j = 0; j <= str2.length; j += 1) track[j][0] = j;
    for (let j = 1; j <= str2.length; j += 1) {
      for (let i = 1; i <= str1.length; i += 1) {
        const indicator = str1[i - 1] === str2[j - 1] ? 0 : 1;
        track[j][i] = Math.min(
          track[j][i - 1] + 1,
          track[j - 1][i] + 1,
          track[j - 1][i - 1] + indicator,
        );
      }
    }
    return track[str2.length][str1.length];
  }

  const getFeedbackColor = (feedback: FeedbackValue) => {
    switch (feedback) {
      case "correct": return "bg-green-500";
      case "related": return "bg-yellow-500";
      case "incorrect": return "bg-gray-500";
    }
  };

  const getCategoryDescription = (category: string): string => {
    switch (category) {
      case "type": return "The classification of the technology (Programming Language, Framework, Library, Tool, etc.)";
      case "paradigm": return "The programming approach or methodology associated with the technology";
      case "domain": return "The primary field or area where the technology is applied";
      case "company": return "The organization, foundation, or entity that created or maintains the technology";
      default: return "";
    }
  };

  const generateShareText = () => {
    const date = new Date().toISOString().split('T')[0]; // Formato YYYY-MM-DD
    const guessesText = guesses.length + '/' + maxAttempts;

    const grid = guesses.map(guess => {
      const yearFeedback = guess.feedback.releaseYear === 'correct'
        ? '🟩'
        : guess.feedback.releaseYear === 'related'
          ? '🟨' + (guess.feedback.yearDirection === 'higher' ? '⬆️' : '⬇️')
          : '⬜' + (guess.feedback.yearDirection === 'higher' ? '⬆️' : '⬇️');

      const typeFeedback = guess.feedback.type === 'correct' ? '🟩' : guess.feedback.type === 'related' ? '🟨' : '⬜';
      const paradigmFeedback = guess.feedback.paradigm === 'correct' ? '🟩' : guess.feedback.paradigm === 'related' ? '🟨' : '⬜';
      const domainFeedback = guess.feedback.domain === 'correct' ? '🟩' : guess.feedback.domain === 'related' ? '🟨' : '⬜';
      const companyFeedback = guess.feedback.company === 'correct' ? '🟩' : guess.feedback.company === 'related' ? '🟨' : '⬜';

      return `${yearFeedback} ${typeFeedback} ${paradigmFeedback} ${domainFeedback} ${companyFeedback}`;
    }).join('\n');

    return `Lang Guessr - ${date}\n` +
           `Code Cracked: ${guessesText}\n` +
           `Master the Language Matrix!\n\n` +
           `${grid}\n\n` +
           `Debug this puzzle - Beat my stack!`;
  };

  const handleShare = () => {
    const shareText = generateShareText();
    navigator.clipboard.writeText(shareText)
      .then(() => {
        toast.success('Results copied to clipboard!');
      })
      .catch(() => {
        toast.error('Failed to copy to clipboard.');
      });
  };

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
                      className={`p-2 cursor-pointer hover:bg-zinc-50 ${index === selectedIndex ? "bg-zinc-100" : ""}`}
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
              className="bg-zinc-900 text-white px-4 py-2 rounded-md"
            >
              Guess
            </Button>
            <Button
              onClick={() => setShowHistoricalHint(true)}
              className="px-4 py-2 rounded-md"
              variant={"outline"}
            >
              Show historical hint
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
            <p>
              You guessed the correct term: <strong>{targetTerm?.name}</strong> in {guesses.length} attempts.<br />
              Come back tomorrow for a new challenge.
            </p>
            <Button onClick={handleShare} className="mt-2 bg-green-600 hover:bg-green-700">
              <Share className="w-2 h-2" /> Share
            </Button>
          </AlertDescription>
        </Alert>
      )}

      {gameStatus === "lost" && (
        <Alert className="mb-6 bg-red-50 border-red-200">
          <AlertCircle className="h-4 w-4 text-red-600" />
          <AlertTitle className="text-red-800">Game Over</AlertTitle>
          <AlertDescription className="text-red-700">
            <p>
              You didn’t guess correctly after {maxAttempts} attempts. The correct term was: <strong>{targetTerm?.name}</strong><br />
              Come back tomorrow for a new challenge.
            </p>
            <Button onClick={handleShare} className="mt-2 bg-red-600 hover:bg-red-700">
              <Share className="w-2 h-2" /> Share
            </Button>
          </AlertDescription>
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

        {gameStatus === "playing" && showHistoricalHint && (
          <div className="mb-6 p-4 bg-gray-100 rounded-lg border">
            <h3 className="font-bold mb-2">Historical Hint:</h3>
            <div className="text-sm font-mono">
              {targetTerm?.historicalHint}
            </div>
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
                  {["type", "paradigm", "domain", "company"].map((category) => {
                    const categoryKey = category as FeedbackKey;
                    return (
                      <td key={category} className="px-2 py-3">
                        <TooltipProvider>
                          <Tooltip>
                            <TooltipTrigger asChild>
                              <div
                                className={`${getFeedbackColor(
                                  guess.feedback[categoryKey]
                                )} text-white rounded-md p-2 text-center`}
                              >
                                {guess.termData[categoryKey as keyof SoftwareTerm]}
                              </div>
                            </TooltipTrigger>
                            <TooltipContent>
                              {gameStatus !== "playing" && guess.feedback[categoryKey] !== "correct" && (
                                <p>Correct answer: {targetTerm && targetTerm[categoryKey as keyof SoftwareTerm]}</p>
                              )}
                            </TooltipContent>
                          </Tooltip>
                        </TooltipProvider>
                      </td>
                    );
                  })}
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
            <span>Guess the daily software term (programming language, framework, library, tool)</span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For the <strong>Year</strong> column: The guessed year is shown with background color:
              <div className="flex items-center mt-1 gap-2">
                <span className="w-4 h-4 bg-green-500 rounded-sm"></span> Exact match
                <span className="w-4 h-4 bg-yellow-500 rounded-sm"></span> Within 3 years
                <span className="w-4 h-4 bg-gray-500 rounded-sm"></span> More than 3 years away
              </div>
              An arrow (↑ or ↓) indicates if the target year is higher or lower when not correct.
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>For other categories: The guessed value is shown with background color:
              <div className="flex items-center mt-1 gap-2">
                <span className="w-4 h-4 bg-green-500 rounded-sm"></span> Exact match
                <span className="w-4 h-4 bg-yellow-500 rounded-sm"></span> Related
                <span className="w-4 h-4 bg-gray-500 rounded-sm"></span> Not related
              </div>
            </span>
          </li>
          <li className="flex items-start">
            <span className="mr-2">•</span>
            <span>You have {maxAttempts} attempts to guess correctly once per day</span>
          </li>
        </ul>
      </div>
    </div>
  )
}