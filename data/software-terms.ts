export type SoftwareTerm = {
  name: string;
  releaseYear: number;
  type: string;
  paradigm: string;
  domain: string;
  company: string;
  codeSnippet: string;
};

export const softwareTerms: SoftwareTerm[] = [
  {
    name: "JavaScript",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Netscape/ECMA",
    codeSnippet: `function greet(name) {\n  let message = "Hello, " + name;\n  console.log(message);\n  return message;\n}`
  },
  {
    name: "Python",
    releaseYear: 1991,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Python Software Foundation",
    codeSnippet: `def greet(name):\n  message = f"Hello, {name}"\n  print(message)\n  return message\n`
  },
  {
    name: "TypeScript",
    releaseYear: 2012,
    type: "Programming Language",
    paradigm: "Static typing",
    domain: "Web Development",
    company: "Microsoft",
    codeSnippet: `function greet(name: string): string {\n  let message: string = "Hello, " + name;\n  console.log(message);\n  return message;\n}`
  },
  {
    name: "Swift",
    releaseYear: 2014,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Mobile Development",
    company: "Apple",
    codeSnippet: `func greet(name: String) -> String {\n  let message = "Hello, " + name\n  print(message)\n  return message\n}`
  },
  {
    name: "Java",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "General Purpose",
    company: "Oracle",
    codeSnippet: `class Hello {\n  public static String greet(String name) {\n    return "Hello, " + name;\n  }\n}`
  },
  {
    name: "C++",
    releaseYear: 1985,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Bell Labs",
    codeSnippet: `#include <string>\nstring greet(string name) {\n  return "Hello, " + name;\n}\n`
  },
  {
    name: "C#",
    releaseYear: 2000,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Microsoft",
    codeSnippet: `class Program {\n  static string Greet(string name) {\n    return "Hello, " + name;\n  }\n}`
  },
  {
    name: "Ruby",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "General Purpose",
    company: "Ruby Association",
    codeSnippet: `def greet(name)\n  message = "Hello, #{name}"\n  puts message\n  message\n`
  },
  {
    name: "Go",
    releaseYear: 2009,
    type: "Programming Language",
    paradigm: "Concurrent",
    domain: "General Purpose",
    company: "Google",
    codeSnippet: `func greet(name string) string {\n  message := "Hello, " + name\n  fmt.Println(message)\n  return message\n}`
  },
  {
    name: "Rust",
    releaseYear: 2010,
    type: "Programming Language",
    paradigm: "Systems",
    domain: "General Purpose",
    company: "Mozilla",
    codeSnippet: `fn greet(name: &str) -> String {\n  let message = format!("Hello, {}", name);\n  println!("{}", message);\n  message\n}`
  },
  {
    name: "Django",
    releaseYear: 2005,
    type: "Framework",
    paradigm: "MVT",
    domain: "Web Development",
    company: "Django Software Foundation",
    codeSnippet: `def hello(request):\n  message = "Hello, World!"\n  return HttpResponse(message)\n# Maps to '/hello' route\n`
  },
  {
    name: "Ruby on Rails",
    releaseYear: 2004,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Basecamp",
    codeSnippet: `def index\n  @message = "Hello, World!"\n  render plain: @message\nend\n`
  },
  {
    name: "Express.js",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "TJ Holowaychuk",
    codeSnippet: `function handler(req, res) {\n  res.send("Hello, World!");\n}\napp.get('/', handler);\n`
  },
  {
    name: "Spring",
    releaseYear: 2002,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Pivotal",
    codeSnippet: `public String hello() {\n  String message = "Hello, World!";\n  return message;\n} // GET /hello\n`
  },
  {
    name: "Flask",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "Microframework",
    domain: "Web Development",
    company: "Armin Ronacher",
    codeSnippet: `def hello():\n  message = "Hello, World!"\n  return message\n# Route: '/' \n`
  },
];