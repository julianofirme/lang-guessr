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
  {
    name: "React Native",
    releaseYear: 2015,
    type: "Framework",
    paradigm: "Component-based",
    domain: "Mobile Development",
    company: "Facebook",
    codeSnippet: `function App() {\n  return <Text>Hello, World!</Text>;\n}\nexport default App;\n`
  },
  {
    name: "Flutter",
    releaseYear: 2017,
    type: "Framework",
    paradigm: "Reactive",
    domain: "Mobile Development",
    company: "Google",
    codeSnippet: `class MyApp extends StatelessWidget {\n  Widget build(context) {\n    return Text('Hello, World!');\n  }\n}`
  },
  {
    name: "Spring Boot",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Pivotal",
    codeSnippet: `@GetMapping("/hello")\npublic String hello() {\n  return "Hello, World!";\n}\n`
  },
  {
    name: "Laravel",
    releaseYear: 2011,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Taylor Otwell",
    codeSnippet: `Route::get('/hello', function () {\n  return 'Hello, World!';\n});\n// Route handler\n`
  },
  {
    name: "React",
    releaseYear: 2013,
    type: "Framework",
    paradigm: "Component-based",
    domain: "Web Development",
    company: "Facebook",
    codeSnippet: `function Hello() {\n  return <div>Hello, World!</div>;\n}\nexport default Hello;\n`
  },
  {
    name: "Angular",
    releaseYear: 2010,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Google",
    codeSnippet: `@Component({\n  selector: 'app-hello',\n  template: '<div>Hello, World!</div>'\n})\n`
  },
  {
    name: "Vue.js",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "Progressive",
    domain: "Web Development",
    company: "Evan You",
    codeSnippet: `<template>\n  <div>Hello, World!</div>\n</template>\n<script>export default {}</script>\n`
  },
  {
    name: "Svelte",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "Compiler-based",
    domain: "Web Development",
    company: "Rich Harris",
    codeSnippet: `<script>let msg = 'Hello, World!';</script>\n<div>{msg}</div>\n<!-- Reactive UI -->\n`
  },
  {
    name: "Next.js",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "SSR",
    domain: "Web Development",
    company: "Vercel",
    codeSnippet: `export default function Home() {\n  return <div>Hello, World!</div>;\n}\n// Server-side rendered\n`
  },
  {
    name: "Nuxt.js",
    releaseYear: 2016,
    type: "Framework",
    paradigm: "SSR",
    domain: "Web Development",
    company: "Nuxt Labs",
    codeSnippet: `<template>\n  <div>Hello, World!</div>\n</template>\n<!-- Server-side rendered -->\n`
  },
  {
    name: "Bash",
    releaseYear: 1989,
    type: "Programming Language",
    paradigm: "Scripting",
    domain: "System Administration",
    company: "GNU Project",
    codeSnippet: `greet() {\n  local msg="Hello, $1"\n  echo "$msg"\n  return 0\n}\n`
  },
  {
    name: "Zig",
    releaseYear: 2016,
    type: "Programming Language",
    paradigm: "Systems",
    domain: "General Purpose",
    company: "Andrew Kelley",
    codeSnippet: `fn greet(name: []const u8) ![]const u8 {\n  const msg = "Hello, " ++ name;\n  std.debug.print("{s}\\n", .{msg});\n  return msg;\n}`
  },
  {
    name: "C",
    releaseYear: 1972,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "General Purpose",
    company: "Bell Labs",
    codeSnippet: `#include <stdio.h>\nvoid greet(char* name) {\n  printf("Hello, %s\\n", name);\n}\n`
  },
  {
    name: "PHP",
    releaseYear: 1994,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Zend",
    codeSnippet: `function greet($name) {\n  $msg = "Hello, $name";\n  echo $msg;\n  return $msg;\n}`
  },
  {
    name: "Kotlin",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Mobile Development",
    company: "JetBrains",
    codeSnippet: `fun greet(name: String): String {\n  val msg = "Hello, $name"\n  println(msg)\n  return msg\n}`
  },
  {
    name: "Scala",
    releaseYear: 2004,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "EPFL",
    codeSnippet: `def greet(name: String): String = {\n  val msg = s"Hello, $name"\n  println(msg)\n  msg\n}`
  },
  {
    name: "Haskell",
    releaseYear: 1990,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Haskell Committee",
    codeSnippet: `greet name = do\n  let msg = "Hello, " ++ name\n  putStrLn msg\n  return msg\n`
  },
  {
    name: "Livewire",
    releaseYear: 2019,
    type: "Framework",
    paradigm: "Real-time",
    domain: "Web Development",
    company: "Caleb Porzio",
    codeSnippet: `public $message = 'Hello, World!';\n// Blade: {{ $message }}\n<!-- Live updates -->\n`
  },
  {
    name: "HTMX",
    releaseYear: 2020,
    type: "Framework",
    paradigm: "HTML-centric",
    domain: "Web Development",
    company: "Big Sky Software",
    codeSnippet: `<div hx-get="/hello">\n  Hello, World!\n</div>\n<!-- AJAX trigger -->\n`
  },
  {
    name: "Flutter",
    releaseYear: 2017,
    type: "Framework",
    paradigm: "Reactive",
    domain: "Mobile Development",
    company: "Google",
    codeSnippet: `class MyApp extends StatelessWidget {\n  Widget build(context) {\n    return Text('Hello, World!');\n  }\n}`
  },
  {
    name: "Phoenix",
    releaseYear: 2014,
    type: "Framework",
    paradigm: "MVC",
    domain: "Web Development",
    company: "Chris McCord",
    codeSnippet: `def index(conn, _params) do\n  render(conn, "Hello, World!")\nend\n# Controller\n`
  },
  {
    name: "SQL",
    releaseYear: 1974,
    type: "Programming Language",
    paradigm: "Declarative",
    domain: "Database",
    company: "Various",
    codeSnippet: `SELECT 'Hello, ' || name AS greeting\nFROM users\nWHERE id = 1;\n-- Returns greeting string\n`
  },
  {
    name: "OCaml",
    releaseYear: 1996,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "INRIA",
    codeSnippet: `let greet name =\n  let msg = "Hello, " ^ name in\n  print_endline msg;\n  msg\n`
  },
  {
    name: "Julia",
    releaseYear: 2012,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Scientific Computing",
    company: "Julia Computing",
    codeSnippet: `function greet(name)\n  msg = "Hello, $name"\n  println(msg)\n  return msg\nend\n`
  },
  {
    name: "Clojure",
    releaseYear: 2007,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Rich Hickey",
    codeSnippet: `(defn greet [name]\n  (let [msg (str "Hello, " name)]\n    (println msg)\n    msg))\n`
  },
  {
    name: "Assembly",
    releaseYear: 1947,
    type: "Programming Language",
    paradigm: "Low-level",
    domain: "Systems Programming",
    company: "Various",
    codeSnippet: `mov ah, 09h\nlea dx, msg\nint 21h\nret\n`
  },
  {
    name: "Erlang",
    releaseYear: 1986,
    type: "Programming Language",
    paradigm: "Concurrent",
    domain: "Telecommunications",
    company: "Ericsson",
    codeSnippet: `-module(hello).\n-export([greet/1]).\ngreet(Name) -> io:format("Hello, ~s~n", [Name]).\n`
  },
  {
    name: "Elixir",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "Web Development",
    company: "José Valim",
    codeSnippet: `def greet(name) do\n  msg = "Hello, #{name}"\n  IO.puts(msg)\n  msg\nend\n`
  },
  {
    name: "F#",
    releaseYear: 2005,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "Microsoft",
    codeSnippet: `let greet name =\n  let msg = "Hello, " + name\n  printfn "%s" msg\n  msg\n`
  },
  {
    name: "Lisp",
    releaseYear: 1958,
    type: "Programming Language",
    paradigm: "Functional",
    domain: "General Purpose",
    company: "MIT",
    codeSnippet: `(defun greet (name)\n  (let ((msg (concatenate 'string "Hello, " name)))\n    (print msg)\n    msg))\n`
  },
  {
    name: "Fortran",
    releaseYear: 1957,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "Scientific Computing",
    company: "IBM",
    codeSnippet: `subroutine greet(name)\n  character*(*) :: name\n  print *, 'Hello, ', name\nend subroutine\n`
  },
  {
    name: "COBOL",
    releaseYear: 1959,
    type: "Programming Language",
    paradigm: "Imperative",
    domain: "Business Applications",
    company: "CODASYL",
    codeSnippet: `DISPLAY 'Hello, ' NAME\nMOVE 'Hello, ' TO MSG\nADD 1 TO COUNTER\nDISPLAY MSG\n`
  },
  {
    name: "Perl",
    releaseYear: 1987,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "General Purpose",
    company: "Larry Wall",
    codeSnippet: `sub greet {\n  my $name = shift;\n  print "Hello, $name\\n";\n  return "Hello, $name";\n}`
  },
  {
    name: "Lua",
    releaseYear: 1993,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Embedded Systems",
    company: "PUC-Rio",
    codeSnippet: `function greet(name)\n  local msg = "Hello, " .. name\n  print(msg)\n  return msg\nend\n`
  },
  {
    name: "Dart",
    releaseYear: 2011,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Web Development",
    company: "Google",
    codeSnippet: `String greet(String name) {\n  var msg = 'Hello, $name';\n  print(msg);\n  return msg;\n}`
  },
  {
    name: "Objective-C",
    releaseYear: 1984,
    type: "Programming Language",
    paradigm: "Object-oriented",
    domain: "Mobile Development",
    company: "Apple",
    codeSnippet: `- (NSString *)greet:(NSString *)name {\n  NSString *msg = [@"Hello, " stringByAppendingString:name];\n  NSLog(@"%@", msg);\n  return msg;\n}`
  },
  {
    name: "Ada",
    releaseYear: 1980,
    type: "Programming Language",
    paradigm: "Multi-paradigm",
    domain: "Systems Programming",
    company: "U.S. Department of Defense",
    codeSnippet: `procedure Greet(Name : String) is\nbegin\n  Put_Line("Hello, " & Name);\nend Greet;\n`
  },
];