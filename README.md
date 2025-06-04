# Runestone : Local PostgreSQL DB Hub with Docker

A lightweight, script-powered local PostgreSQL container for all your dev databases. Switch, dump, restore, and manage Postgres DBs with ease from your terminal.

> 💡 Built for testing, local development, and fast prototyping , not production.

## Features

- Dockerized PostgreSQL 14
- Central hub to manage **multiple databases**
- Easy-to-use bash scripts for:
  - Creating / Deleting databases
  - Dumping / Restoring `.sql` files
  - Listing & entering databases
- `.env` powered setup
Designed for:
- Local dev & testing
- Playing with .sql dumps
- API prototyping with fresh DBs
- Team setups with easy onboarding



What Runestone is Not
----
While Runestone is powerful for local development, it’s important to understand its scope and limitations:
- Not a GUI client
Runestone provides terminal-based control it doesn’t offer visual tools like DBeaver, TablePlus, or pgAdmin.
-  Not for production
This setup is meant for local development only. It lacks production-grade security, backups, and high-availability guarantees.
-  No clustering or replication support
Runestone doesn’t handle distributed database setups like primary-replica clusters, sharding, or replication.
- Not a cloud DB replacement
It doesn’t compete with services like Amazon RDS, Google Cloud SQL, or Supabaseit’s a testing sandbox.
- PostgreSQL only
This is a Postgres-only setup. It does not support MySQL, MongoDB, Redis, or other databases out of the box.

### Use Case
---
You’re building 3 side projects,all use PostgreSQL. You want to:
- Test against different DBs without polluting your global system
- Reset DBs quickly with fresh .sql data
- Keep backups easily
- Avoid installing Postgres directly
- Keep local backups organized

That’s where Runestone comes in one container, infinite databases.

## Folder Structure
    runestone/
    ├── .env.example
    ├── docker-compose.yml
    ├── backups/               # All DB dumps saved here
    └── scripts/               # All utility bash scripts
        ├── create_db.sh
        ├── delete_db.sh
        ├── dump_db.sh
        ├── enter_db.sh
        ├── list_db.sh
        └── restore_db.sh

---

## Getting Started

1. Clone this repo

```bash
git clone https://github.com/ganidande905/runestone.git
cd runestone
```
Make sure all scripts inside scripts/ folder are executable:
```
chmod +x scripts/*.sh
```
2. Set up your .env and .gitignore

```
cp .env.example .env
```
Edit .env with your DB name, user, password, and container name

3. Add These to Your .zshrc / .bashrc
 ```
 if [ -f ~/FOSS/runestone/.env ]; then
  set -a
  source ~/FOSS/runestone/.env
  set +a
fi

 # Open container shell
alias de="docker exec -it runestone bash"

# DB scripts
alias pgc="~/runestone/scripts/create_db.sh"
alias pgrm="~/runestone/scripts/delete_db.sh"
alias pgl="~/runestone/scripts/list_db.sh"
alias pge="~/runestone/scripts/enter_db.sh"
alias pgdump="~/runestone/scripts/dump_db.sh"
alias pgr="~/runestone/scripts/restore_db.sh"

 ```
Then reload:
```
source ~/.zshrc  # or ~/.bashrc
```
4. Start PostgreSQL container
```
docker-compose up -d
```

## Command Aliases

| Alias     | Description                            | Script Used             |
|-----------|----------------------------------------|--------------------------|
| `de`      | Open bash shell inside the container   | `docker exec -it $CONTAINER_NAME bash` |
| `pgc`     | Create a new PostgreSQL database       | `scripts/create_db.sh`  |
| `pgrm`    | Delete an existing database            | `scripts/delete_db.sh`  |
| `pgl`     | List all databases in the container    | `scripts/list_db.sh`    |
| `pge`     | Enter a specific database via `psql`   | `scripts/enter_db.sh`   |
| `pgdump`  | Dump/export a database to `.sql`       | `scripts/dump_db.sh`    |
| `pgr`     | Restore/import `.sql` into a database  | `scripts/restore_db.sh` |

> 💡 All aliases use `.env` variables like `PGUSER`, `PGPORT`, `CONTAINER_NAME`, etc.

Why “Runestone”?
---
The name Runestone draws inspiration from ancient runestones — carved stones inscribed with runes that preserved important messages, histories, or magic in a tangible, enduring form.

Similarly, this project is designed to be a magical keeper of your local databases, a sturdy and reliable “stone” that holds the vital information for your development workflows. It’s a place where your data “stories” live safely, accessible anytime you summon them.

Just like runestones were tools for communication and memory in olden times, Runestone is your modern developer’s tool to manage, store, and restore your data effortlessly — transforming complex database tasks into simple, repeatable rituals.