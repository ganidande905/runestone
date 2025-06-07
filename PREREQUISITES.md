# 🔧 Prerequisites for Running Runestone

Runestone is a Haskell-based CLI utility to manage local PostgreSQL databases via Docker. To use it effectively, ensure the following dependencies are installed on your system.

---

## 🍎 macOS Prerequisites

### 1. Install Homebrew (if not already)
```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

### 2. Install GHC, Stack & Docker
```bash
brew install ghc stack libpq
brew install --cask docker
```

> ✅ Start Docker from Applications and ensure it’s running.

### 3. Add `pg_config` to path if needed
```bash
echo 'export PATH="/opt/homebrew/opt/libpq/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

---

## 🐧 Linux Prerequisites

### 1. Install GHC & Stack
```bash
sudo apt update
sudo apt install -y curl libpq-dev postgresql-client
curl -sSL https://get.haskellstack.org/ | sh
```

> ✅ `libpq-dev` provides `pg_config` needed for building `postgresql-simple`.

### 2. Install Docker
```bash
sudo apt install -y docker.io
sudo systemctl start docker
sudo usermod -aG docker $USER
```

> You may need to restart your terminal or logout-login for group changes to take effect.

---

## 🪟 Windows Prerequisites

### 1. Install Chocolatey (if not already)
Open PowerShell as Administrator:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force; `
[System.Net.ServicePointManager]::SecurityProtocol = `
[System.Net.ServicePointManager]::SecurityProtocol -bor 3072; `
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))
```

### 2. Install GHC, Stack & Docker
```powershell
choco install haskell-stack docker-desktop -y
```

> ✅ Make sure Docker Desktop is running before using Runestone.

---

## 📦 Additional Setup Notes

- Ensure `pg_config` is available in your `PATH`.
  - `which pg_config` (Linux/macOS) or `where pg_config` (Windows).
- Allow Stack to install GHC when prompted.
- Ensure `.env` file with PostgreSQL credentials exists before running any command.
- Docker should be installed and running with a container named `runestone` for the CLI to work.

---

## ✅ Example .env File

```
PGUSER=postgres
PGPASSWORD=yourpassword
PGHOST=localhost
```

---

## 👌 Once Ready

Build the project:
```bash
stack build
```

Run using:
```bash
stack exec runestone -- create-db
```

Or use defined aliases (optional).