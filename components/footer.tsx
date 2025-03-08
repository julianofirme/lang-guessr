import Link from "next/link"
import { Github, Twitter } from "lucide-react"

export function Footer() {
  return (
    <footer className="w-full p-4 mt-auto">
      <div className="container flex items-center justify-center">
        <div className="flex items-center gap-4">
          <Link
            href="https://github.com/julianofirme/lang-guessr"
            target="_blank"
            rel="noopener noreferrer"
            aria-label="GitHub repository"
            className="text-muted-foreground hover:text-foreground transition-colors"
          >
            <Github className="h-4 w-4" />
          </Link>
          <Link
            href="https://x.com/f1rme"
            target="_blank"
            rel="noopener noreferrer"
            aria-label="X (Twitter) profile"
            className="text-muted-foreground hover:text-foreground transition-colors"
          >
            <Twitter className="h-4 w-4" />
          </Link>
        </div>
      </div>
    </footer>
  )
}

