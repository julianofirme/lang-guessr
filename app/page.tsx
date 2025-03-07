import SoftwareWordle from "@/components/software-wordle"

export default function Home() {
  return (
    <main className="flex min-h-screen flex-col items-center justify-between p-4 md:p-12">
      <div className="z-10 w-full max-w-5xl items-center justify-center font-mono text-sm">
        <h1 className="mb-8 text-center text-3xl font-bold">lang_guessr</h1>
        <p className="mb-8 text-center text-lg">
          Guess the software term based on its categories. You have 6 attempts.
        </p>
        <SoftwareWordle />
      </div>
    </main>
  )
}

