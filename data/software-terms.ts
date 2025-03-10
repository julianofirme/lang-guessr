export type SoftwareTerm = {
  name: string;
  releaseYear: number;
  type: string;
  paradigm: string;
  domain: string;
  company: string;
  codeSnippet: string;
  historicalHint: string;
};

export const softwareTerms: SoftwareTerm[] = [
  {
    name: "JavaScript",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Netscape/ECMA",
    codeSnippet: `function greet(name) {\n  let message = "Hello, " + name;\n  console.log(message);\n  return message;\n}`,
    historicalHint: "Developed in just 10 days by a programmer at a pioneering web company, this technology transformed static web pages into interactive experiences."
  },
  {
    name: "Python",
    releaseYear: 1991,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Python Software Foundation",
    codeSnippet: `def greet(name):\n  message = f"Hello, {name}"\n  print(message)\n  return message\n`,
    historicalHint: "Inspired by a British comedy troupe, this creation from the early 1990s became renowned for its clear syntax and versatility across domains."
  },
  {
    name: "TypeScript",
    releaseYear: 2012,
    type: "Programming Language",
    paradigm: "Static typing",
    domain: "Web Development",
    company: "Microsoft",
    codeSnippet: `function greet(name: string): string {\n  let message: string = "Hello, " + name;\n  console.log(message);\n  return message;\n}`,
    historicalHint: "Introduced by a major tech firm in the early 2010s, this tool added a layer of type safety to enhance large-scale web projects."
  },
  {
    name: "Swift",
    releaseYear: 2014,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Mobile Development",
    company: "Apple",
    codeSnippet: `func greet(name: String) -> String {\n  let message = "Hello, " + name\n  print(message)\n  return message\n}`,
    historicalHint: "Unveiled at a developer conference in 2014, this innovation replaced an older language to streamline app creation for a popular mobile ecosystem."
  },
  {
    name: "Java",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "General Purpose",
    company: "Oracle",
    codeSnippet: `class Hello {\n  public static String greet(String name) {\n    return "Hello, " + name;\n  }\n}`,
    historicalHint: "Emerging in the mid-1990s from a company later acquired by a database giant, this tool promised portability across platforms with its virtual machine."
  },
  {
    name: "C++",
    releaseYear: 1985,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Bell Labs",
    codeSnippet: `#include <string>\nstring greet(string name) {\n  return "Hello, " + name;\n}\n`,
    historicalHint: "Born in the 1980s at a legendary research lab, this extension of an earlier language added object-oriented features for high-performance software."
  },
  {
    name: "C#",
    releaseYear: 2000,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Microsoft",
    codeSnippet: `class Program {\n  static string Greet(string name) {\n    return "Hello, " + name;\n  }\n}`,
    historicalHint: "Launched at the turn of the millennium by a software titan, this creation was crafted as a rival to a popular cross-platform language."
  },
  {
    name: "Ruby",
    releaseYear: 1995,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "General Purpose",
    company: "Ruby Association",
    codeSnippet: `def greet(name)\n  message = "Hello, #{name}"\n  puts message\n  message\n`,
    historicalHint: "Conceived in the mid-1990s by a Japanese developer, this tool focused on developer joy and later inspired a famous web framework."
  },
  {
    name: "Go",
    releaseYear: 2009,
    type: "Programming Language",
    paradigm: "Concurrent",
    domain: "General Purpose",
    company: "Google",
    codeSnippet: `func greet(name string) string {\n  message := "Hello, " + name\n  fmt.Println(message)\n  return message\n}`,
    historicalHint: "Developed by a trio at a search engine giant in the late 2000s, this language emphasized simplicity and built-in concurrency support."
  },
  {
    name: "Rust",
    releaseYear: 2010,
    type: "Programming Language",
    paradigm: "Systems",
    domain: "General Purpose",
    company: "Mozilla",
    codeSnippet: `fn greet(name: &str) -> String {\n  let message = format!("Hello, {}", name);\n  println!("{}", message);\n  message\n}`,
    historicalHint: "Backed by a browser foundation in the early 2010s, this language tackled memory safety challenges without relying on garbage collection."
  },
  {
    name: "Django",
    releaseYear: 2005,
    type: "Framework",
    paradigm: "MVT",
    domain: "Web Development",
    company: "Django Software Foundation",
    codeSnippet: `def hello(request):\n  message = "Hello, World!"\n  return HttpResponse(message)\n# Maps to '/hello' route\n`,
    historicalHint: "Originating from a newsroom project in the mid-2000s, this tool was released to streamline web development with a comprehensive feature set."
  },
  {
    name: "Ruby on Rails",
    releaseYear: 2004,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Basecamp",
    codeSnippet: `def index\n  @message = "Hello, World!"\n  render plain: @message\nend\n`,
    historicalHint: "Emerging from a project management tool in the early 2000s, this framework introduced conventions that reshaped web development workflows."
  },
  {
    name: "Express.js",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "TJ Holowaychuk",
    codeSnippet: `function handler(req, res) {\n  res.send("Hello, World!");\n}\napp.get('/', handler);\n`,
    historicalHint: "Crafted by an individual developer in 2010, this tool became a cornerstone for server-side development in a popular runtime environment."
  },
  {
    name: "Spring",
    releaseYear: 2002,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Pivotal",
    codeSnippet: `public String hello() {\n  String message = "Hello, World!";\n  return message;\n} // GET /hello\n`,
    historicalHint: "Introduced in the early 2000s by a software pioneer, this framework simplified enterprise development by reducing reliance on complex standards."
  },
  {
    name: "Flask",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "Microframework",
    domain: "Web Development",
    company: "Armin Ronacher",
    codeSnippet: `def hello():\n  message = "Hello, World!"\n  return message\n# Route: '/' \n`,
    historicalHint: "Started as a playful experiment in 2010, this lightweight tool grew into a popular choice for minimalistic web projects."
  },
  {
    name: "React Native",
    releaseYear: 2015,
    type: "Framework",
    paradigm: "Component-based",
    domain: "Mobile Development",
    company: "Facebook",
    codeSnippet: `function App() {\n  return <Text>Hello, World!</Text>;\n}\nexport default App;\n`,
    historicalHint: "Released by a social media giant in 2015, this framework extended a web library’s principles to mobile app creation."
  },
  {
    name: "Flutter",
    releaseYear: 2017,
    type: "Framework",
    paradigm: "Reactive",
    domain: "Mobile Development",
    company: "Google",
    codeSnippet: `class MyApp extends StatelessWidget {\n  Widget build(context) {\n    return Text('Hello, World!');\n  }\n}`,
    historicalHint: "Announced in 2017 by a tech powerhouse, this tool paired a new language with a widget system to challenge traditional mobile development."
  },
  {
    name: "Spring Boot",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Pivotal",
    codeSnippet: `@GetMapping("/hello")\npublic String hello() {\n  return "Hello, World!";\n}\n`,
    historicalHint: "Evolving from an earlier framework in 2014, this tool streamlined microservices with automatic setup and embedded server support."
  },
  {
    name: "Laravel",
    releaseYear: 2011,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Taylor Otwell",
    codeSnippet: `Route::get('/hello', function () {\n  return 'Hello, World!';\n});\n// Route handler\n`,
    historicalHint: "Launched by an individual developer in 2011, this framework brought elegant syntax to a widely used server-side language."
  },
  {
    name: "React",
    releaseYear: 2013,
    type: "Framework",
    paradigm: "Component-based",
    domain: "Web Development",
    company: "Facebook",
    codeSnippet: `function Hello() {\n  return <div>Hello, World!</div>;\n}\nexport default Hello;\n`,
    historicalHint: "Developed by a social network team in 2013, this library introduced a virtual DOM to revolutionize web interface design."
  },
  {
    name: "Angular",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Google",
    codeSnippet: `@Component({\n  selector: 'app-hello',\n  template: '<div>Hello, World!</div>'\n})\n`,
    historicalHint: "First released by a search giant in 2010, this framework evolved from a simpler version into a comprehensive web solution."
  },
  {
    name: "Vue.js",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "Progressive",
    domain: "Web Development",
    company: "Evan You",
    codeSnippet: `<template>\n  <div>Hello, World!</div>\n</template>\n<script>export default {}</script>\n`,
    historicalHint: "Crafted by a lone developer in 2014, this tool offered a lightweight alternative to heavier web frameworks of the time."
  },
  {
    name: "Svelte",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "Compiler-based",
    domain: "Web Development",
    company: "Rich Harris",
    codeSnippet: `<script>let msg = 'Hello, World!';</script>\n<div>{msg}</div>\n<!-- Reactive UI -->\n`,
    historicalHint: "Introduced in 2016 by a journalist-turned-developer, this framework shifted processing from runtime to build time for efficiency."
  },
  {
    name: "Next.js",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "SSR",
    domain: "Web Development",
    company: "Vercel",
    codeSnippet: `export default function Home() {\n  return <div>Hello, World!</div>;\n}\n// Server-side rendered\n`,
    historicalHint: "Launched by a deployment platform in 2016, this tool enhanced a popular library with server-side capabilities."
  },
  {
    name: "Nuxt.js",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "SSR",
    domain: "Web Development",
    company: "Nuxt Labs",
    codeSnippet: `<template>\n  <div>Hello, World!</div>\n</template>\n<!-- Server-side rendered -->\n`,
    historicalHint: "Inspired by another SSR framework in 2016, this tool brought similar benefits to a progressive web library."
  },
  {
    name: "Bash",
    releaseYear: 1989,
    type: "Programming Language",
    paradigm: "Scripting",
    domain: "System Administration",
    company: "GNU Project",
    codeSnippet: `greet() {\n  local msg="Hello, $1"\n  echo "$msg"\n  return 0\n}\n`,
    historicalHint: "Created in the late 1980s under a free software initiative, this tool became the standard scripting environment for Unix systems."
  },
  {
    name: "Zig",
    releaseYear: 2016,
    type: "Programming Language",
    paradigm: "Systems",
    domain: "General Purpose",
    company: "Andrew Kelley",
    codeSnippet: `fn greet(name: []const u8) ![]const u8 {\n  const msg = "Hello, " ++ name;\n  std.debug.print("{s}\\n", .{msg});\n  return msg;\n}`,
    historicalHint: "Debuted in 2016 by an independent developer, this language aimed to modernize systems programming with improved safety features."
  },
  {
    name: "C",
    releaseYear: 1972,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "General Purpose",
    company: "Bell Labs",
    codeSnippet: `#include <stdio.h>\nvoid greet(char* name) {\n  printf("Hello, %s\\n", name);\n}\n`,
    historicalHint: "Emerging from a research lab in the early 1970s, this language underpinned the development of a groundbreaking operating system."
  },
  {
    name: "PHP",
    releaseYear: 1994,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Zend",
    codeSnippet: `function greet($name) {\n  $msg = "Hello, $name";\n  echo $msg;\n  return $msg;\n}`,
    historicalHint: "Starting as a set of personal web tools in the mid-1990s, this technology grew to dominate server-side web scripting."
  },
  {
    name: "Kotlin",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Mobile Development",
    company: "JetBrains",
    codeSnippet: `fun greet(name: String): String {\n  val msg = "Hello, $name"\n  println(msg)\n  return msg\n}`,
    historicalHint: "Introduced by a tools company in 2011, this language became a favored choice for a major mobile platform’s app development."
  },
  {
    name: "Scala",
    releaseYear: 2004,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "EPFL",
    codeSnippet: `def greet(name: String): String = {\n  val msg = s"Hello, $name"\n  println(msg)\n  msg\n}`,
    historicalHint: "Developed in the early 2000s at a Swiss university, this tool merged functional and object-oriented styles on a popular virtual machine."
  },
  {
    name: "Haskell",
    releaseYear: 1990,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Haskell Committee",
    codeSnippet: `greet name = do\n  let msg = "Hello, " ++ name\n  putStrLn msg\n  return msg\n`,
    historicalHint: "Formed by a committee in 1990, this language advanced pure functional programming and influenced academic research."
  },
  {
    name: "Livewire",
    releaseYear: 2019,
    type: "Framework",
    paradigm: "Real-time",
    domain: "Web Development",
    company: "Caleb Porzio",
    codeSnippet: `public $message = 'Hello, World!';\n// Blade: {{ $message }}\n<!-- Live updates -->\n`,
    historicalHint: "Created in 2019 by an indie developer, this tool added real-time features to a popular PHP ecosystem with minimal JavaScript."
  },
  {
    name: "HTMX",
    releaseYear: 2020,
    type: "Framework",
    paradigm: "HTML-centric",
    domain: "Web Development",
    company: "Big Sky Software",
    codeSnippet: `<div hx-get="/hello">\n  Hello, World!\n</div>\n<!-- AJAX trigger -->\n`,
    historicalHint: "Launched in 2020 by a small team, this tool extended HTML to handle dynamic updates without heavy scripting."
  },
  {
    name: "Phoenix",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Chris McCord",
    codeSnippet: `def index(conn, _params) do\n  render(conn, "Hello, World!")\nend\n# Controller\n`,
    historicalHint: "Introduced in 2014 by an individual developer, this framework harnessed a concurrent language for real-time web performance."
  },
  {
    name: "SQL",
    releaseYear: 1974,
    type: "Programming Language",
    paradigm: "Declarative",
    domain: "Database",
    company: "Various",
    codeSnippet: `SELECT 'Hello, ' || name AS greeting\nFROM users\nWHERE id = 1;\n-- Returns greeting string\n`,
    historicalHint: "Developed in the mid-1970s by a tech giant, this language standardized how data is queried from relational systems."
  },
  {
    name: "OCaml",
    releaseYear: 1996,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "INRIA",
    codeSnippet: `let greet name =\n  let msg = "Hello, " ^ name in\n  print_endline msg;\n  msg\n`,
    historicalHint: "Evolving from an earlier project in 1996 at a French research institute, this tool shaped functional programming with strong typing."
  },
  {
    name: "Julia",
    releaseYear: 2012,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Scientific Computing",
    company: "Julia Computing",
    codeSnippet: `function greet(name)\n  msg = "Hello, $name"\n  println(msg)\n  return msg\nend\n`,
    historicalHint: "Launched in 2012 for numerical tasks, this language aimed to blend ease of use with high performance for scientific work."
  },
  {
    name: "Clojure",
    releaseYear: 2007,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Rich Hickey",
    codeSnippet: `(defn greet [name]\n  (let [msg (str "Hello, " name)]\n    (println msg)\n    msg))\n`,
    historicalHint: "Introduced in 2007 by a single developer, this tool modernized an ancient language family for a widely used virtual machine."
  },
  {
    name: "Assembly",
    releaseYear: 1947,
    type: "Programming Language",
    paradigm: "Low-level",
    domain: "Systems Programming",
    company: "Various",
    codeSnippet: `mov ah, 09h\nlea dx, msg\nint 21h\nret\n`,
    historicalHint: "Pioneered in the late 1940s, this approach allowed programmers to directly manipulate hardware in the earliest computers."
  },
  {
    name: "Erlang",
    releaseYear: 1986,
    type: "Programming Language",
    paradigm: "Concurrent",
    domain: "Telecommunications",
    company: "Ericsson",
    codeSnippet: `-module(hello).\n-export([greet/1]).\ngreet(Name) -> io:format("Hello, ~s~n", [Name]).\n`,
    historicalHint: "Developed in the mid-1980s by a telecom company, this language introduced a model for fault-tolerant systems."
  },
  {
    name: "Elixir",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "Web Development",
    company: "José Valim",
    codeSnippet: `def greet(name) do\n  msg = "Hello, #{name}"\n  IO.puts(msg)\n  msg\nend\n`,
    historicalHint: "Created in 2011 by a developer building on an older system, this tool brought modern syntax to scalable web applications."
  },
  {
    name: "F#",
    releaseYear: 2005,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Microsoft",
    codeSnippet: `let greet name =\n  let msg = "Hello, " + name\n  printfn "%s" msg\n  msg\n`,
    historicalHint: "Released in 2005 by a software giant, this language adapted functional concepts for a popular development ecosystem."
  },
  {
    name: "Lisp",
    releaseYear: 1958,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "MIT",
    codeSnippet: `(defun greet (name)\n  (let ((msg (concatenate 'string "Hello, " name)))\n    (print msg)\n    msg))\n`,
    historicalHint: "Invented in the late 1950s at a leading research institution, this language pioneered ideas that shaped AI and modern programming."
  },
  {
    name: "Fortran",
    releaseYear: 1957,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "Scientific Computing",
    company: "IBM",
    codeSnippet: `subroutine greet(name)\n  character*(*) :: name\n  print *, 'Hello, ', name\nend subroutine\n`,
    historicalHint: "Introduced in the late 1950s by a computing pioneer, this language transformed how engineers tackled complex calculations."
  },
  {
    name: "COBOL",
    releaseYear: 1959,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "Business Applications",
    company: "CODASYL",
    codeSnippet: `DISPLAY 'Hello, ' NAME\nMOVE 'Hello, ' TO MSG\nADD 1 TO COUNTER\nDISPLAY MSG\n`,
    historicalHint: "Designed in the late 1950s by a committee including a computing legend, this tool dominated business data processing for decades."
  },
  {
    name: "Perl",
    releaseYear: 1987,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Larry Wall",
    codeSnippet: `sub greet {\n  my $name = shift;\n  print "Hello, $name\\n";\n  return "Hello, $name";\n}`,
    historicalHint: "Born in the late 1980s from a linguist’s vision, this language earned a reputation as a versatile scripting tool for the early web."
  },
  {
    name: "Lua",
    releaseYear: 1993,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Embedded Systems",
    company: "PUC-Rio",
    codeSnippet: `function greet(name)\n  local msg = "Hello, " .. name\n  print(msg)\n  return msg\nend\n`,
    historicalHint: "Developed in the early 1990s at a Brazilian university, this language found its niche in lightweight, embeddable scripting."
  },
  {
    name: "Dart",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Google",
    codeSnippet: `String greet(String name) {\n  var msg = 'Hello, $name';\n  print(msg);\n  return msg;\n}`,
    historicalHint: "Launched in 2011 by a search leader, this language shifted focus to power a mobile framework after an initial web ambition."
  },
  {
    name: "Objective-C",
    releaseYear: 1984,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "Mobile Development",
    company: "Apple",
    codeSnippet: `- (NSString *)greet:(NSString *)name {\n  NSString *msg = [@"Hello, " stringByAppendingString:name];\n  NSLog(@"%@", msg);\n  return msg;\n}`,
    historicalHint: "Adopted by a tech innovator in the 1980s, this language drove early graphical interfaces before being succeeded by a newer rival."
  },
  {
    name: "Ada",
    releaseYear: 1980,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Systems Programming",
    company: "U.S. Department of Defense",
    codeSnippet: `procedure Greet(Name : String) is\nbegin\n  Put_Line("Hello, " & Name);\nend Greet;\n`,
    historicalHint: "Commissioned in 1980 by a military entity, this language prioritized reliability for safety-critical applications like defense systems."
  },
];