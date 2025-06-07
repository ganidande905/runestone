# Runestone : Local PostgreSQL DB Hub with Docker + Haskell

A lightweight, script-powered local PostgreSQL container for all your dev databases — now enhanced with **native Haskell support** for clean, type-safe, CLI-based DB management.

> Built for testing, local development, and fast prototyping — not production.

---

## What’s New in This Version?

> **Runestone is the first of its kind**: a Haskell-powered, CLI-first, terminal-native PostgreSQL management tool for a Dockerized local development environment.

---

## Why Haskell?

Unlike Python or Bash, Haskell offers unique advantages for building robust CLI tools like Runestone:

| Feature               | Why it helps                                   |
|----------------------|------------------------------------------------|
| **Type Safety**        | Catches bugs at compile time, reducing runtime crashes |
| **Native Speed**       | Compiles to fast binaries with C-level performance |
| **Functional Style**   | Encourages clear, modular, and testable code |
| **Small Binaries**     | Produces standalone executables, no runtime needed |
| **Crash Prevention**   | Pure functions + strong types = fewer surprises |

These traits make Haskell ideal for CLI tools that interact with critical services like databases — giving you both **performance and peace of mind**.
##  How It’s Implemented


Runestone is implemented as a single command-line binary with subcommands.

- Each subcommand (e.g. `create-db`, `list-db`, `enter-db`) is implemented as a Haskell function.
- Internally, these are routed from `Main.hs` using pattern matching on CLI arguments.
- The `src/DB` directory contains modular logic for each feature.
### Tech Stack:
- [`postgresql-simple`](https://hackage.haskell.org/package/postgresql-simple): Idiomatic and easy-to-use Postgres client library.
- [`dotenv`](https://hackage.haskell.org/package/dotenv): Reads `.env` configs so credentials are not hardcoded.

---

### Why We Chose `postgresql-simple` over `hasql`

While **Hasql** is known for its high-performance, binary-protocol-based PostgreSQL driver — ideal for **low-level, type-safe, and high-throughput** applications — we decided to switch to **`postgresql-simple`** for this CLI utility after facing several real-world development challenges.

####  Challenges Faced with Hasql

- **Deprecation Confusion**: Many parts of `hasql-connection` (like `settings`) were deprecated or removed in newer versions (e.g., `1.9+`), with unclear upgrade paths.
- **Documentation Gaps**: Official documentation and working examples are outdated or missing for the latest versions.
- **Complex Connection Setup**: Connecting with `.env` variables was unintuitive and broke in newer `hasql` versions.
- **Opaque Error Types**: Handling errors like `SessionError` and `QueryError` required more boilerplate and added friction during development.
- **Tooling Incompatibility**: Integration with common utilities like `dotenv` was painful, and type mismatches caused long debug sessions.

#### Why `postgresql-simple` Works Better Here

- **Quick Setup**: Simple connection using environment variables with `defaultConnectInfo`.
- **Dynamic SQL Support**: Supports `fromString` for composing queries interactively — perfect for CLI tools.
- **Beginner-Friendly**: Simpler type system, easier error handling, and faster prototyping.
- **Wider Community Support**: More up-to-date tutorials, docs, and answers available online.

> We may revisit `hasql` in the future for advanced use cases such as batch operations or when performance becomes a critical concern. But for now, `postgresql-simple` gives us the **right balance of simplicity and power** for building developer-facing CLI tools.
---

## Benefits of Haskell in Runestone

| Benefit | Description |
|--------|-------------|
| Strong Types | Avoid common DB issues at compile-time |
| Fast | Compiled to native code — no interpreter overhead |
| Modular | Each command is a standalone executable |
| Cleaner Code | No more parsing Bash script edge cases |
| Testable | Each binary can be independently tested |
| Safer | Fewer runtime crashes, cleaner resource handling |

---

## Features

- Dockerized PostgreSQL 14
- Central hub to manage **multiple dev databases**
-  Native Haskell CLI tools for:
  - Creating / Deleting databases
  - Dumping / Restoring `.sql` files
  - Listing & entering DBs
- `.env`-powered configuration
- Ultra-fast local development setup without installing PostgreSQL on your machine

---

## Folder Structure

```
runestone/
├── .env.example
├── docker-compose.yml
├── backups/
└── simple/
    ├── app/
    ├── src/DB/
    ├── package.yaml
    └── stack.yaml
```


## Prerequisites

See [`PREREQUISITES.md`](./PREREQUISITES.md) for full setup.


## Getting Started

```bash
git clone https://github.com/ganidande905/runestone.git
cd runestone
cp .env.example .env
cd simple
stack install
```

Ensure you add this to `~/.zshrc` or `~/.bashrc`:

```bash
export PATH="$HOME/.local/bin:$PATH"
```
Then:
```
source ~/.zshrc # or source ~/.bashrc
```

Then:

```bash
docker-compose up -d
```

---
## Optional Aliases

For faster access, you can define the following aliases in your shell config (`.zshrc`, `.bashrc`, etc.):

```bash
# PostgreSQL Docker Exec
alias de="docker exec -it runestone bash"

# Haskell-powered CLI shortcuts
alias pgc="runestone create-db"
alias pgrm="runestone delete-db"
alias pgl="runestone list-db"
alias pge="runestone enter-db"
alias pgdump="runestone dump-db"
alias pgr="runestone restore-db"
```

After adding these, reload your shell:

```bash
source ~/.zshrc  # or ~/.bashrc
```

## Why “Runestone”?

Runestone draws inspiration from ancient runestones—durable, magical artifacts carved with symbols to preserve histories and messages. Similarly, Runestone is your modern tool to "inscribe" and manage your local PostgreSQL databases, ensuring your dev data remains accessible, organized, and easily restored. With a touch of Haskell magic, every command is type-safe, efficient, and ready for your next project.

> **Tagline:** One container, infinite dev databases—etched in Haskell, carved in stone.

---

## Is This the First of Its Kind?

Yes. Runestone is pioneering the approach of using **Haskell** to manage PostgreSQL in a local Dockerized development workflow. Most other tools use Bash, Python, or Go. Runestone uniquely delivers type safety, CLI ergonomics, and modular structure through a purely functional paradigm.

---

## License

[MIT License](LICENSE)

---

## Support

For issues, please open a GitHub issue.

---


