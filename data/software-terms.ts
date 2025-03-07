// Define more meaningful and intuitive categories for software terms
export type SoftwareTerm = {
  name: string
  releaseYear: number
  type: string       // Programming Language, Framework, Library, Tool, Database, etc.
  paradigm: string   // OOP, Functional, Procedural, Declarative, etc. (or "N/A" for non-languages)
  domain: string     // Web, Mobile, Data Science, DevOps, Gaming, etc.
  company: string    // Organization behind it (or "Community" or "Individual")
}

// Sample data with improved categories
export const softwareTerms: SoftwareTerm[] = [
  {
    name: "JavaScript",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Netscape/ECMA"
  },
  {
    name: "React",
    releaseYear: 2013,
    type: "Library",
    paradigm: "Component-based",
    domain: "Web Development",
    company: "Facebook"
  },
  {
    name: "Python",
    releaseYear: 1991,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Python Software Foundation"
  },
  {
    name: "TensorFlow",
    releaseYear: 2015,
    type: "Library",
    paradigm: "Dataflow",
    domain: "Machine Learning",
    company: "Google"
  },
  {
    name: "Docker",
    releaseYear: 2013,
    type: "Tool",
    paradigm: "N/A",
    domain: "DevOps",
    company: "Docker, Inc."
  },
  {
    name: "TypeScript",
    releaseYear: 2012,
    type: "Programming Language",
    paradigm: "Static typing",
    domain: "Web Development",
    company: "Microsoft"
  },
  {
    name: "MongoDB",
    releaseYear: 2009,
    type: "Database",
    paradigm: "NoSQL",
    domain: "Data Storage",
    company: "MongoDB Inc."
  },
  {
    name: "Angular",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "Component-based",
    domain: "Web Development",
    company: "Google"
  },
  {
    name: "Kubernetes",
    releaseYear: 2014,
    type: "Platform",
    paradigm: "N/A",
    domain: "Container Orchestration",
    company: "Google"
  },
  {
    name: "Swift",
    releaseYear: 2014,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Mobile Development",
    company: "Apple"
  }
]