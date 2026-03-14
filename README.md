# freebox.el

Emacs client for [FreeBox](https://github.com/kknifer7/FreeBox).
Search and stream video sources directly inside Emacs using empv/mpv for playback.

**Requires**: FreeBox backend with new HTTP REST API (see below).

---

## Architecture

This is a **two-component system**:

1. **FreeBox Backend** (Java)
   - Runs as headless service: `./FreeBox_*.AppImage --headless`
   - Exposes REST API endpoints at `http://127.0.0.1:9978/api/`
   - Manages video sources, handles search, categories, playback URLs

2. **freebox.el Client** (Emacs Lisp)
   - Pure HTTP API client (no WebSocket)
   - Interactive UI with completing-read menus
   - Transient-based main menu for easy navigation

---

## Setup

### Step 1: Deploy FreeBox with HTTP API

The default FreeBox release does **not** include the Emacs HTTP API endpoints.
You need to:

**Option A: Build from patched source**
```bash
cd /path/to/FreeBox
# Apply patch that adds EmacsFrontendHandler
# (See /home/lynx/git/FreeBox/src/main/java/io/knifer/freebox/net/http/handler/EmacsFrontendHandler.java)
./gradlew build
./gradlew run --args="--headless"
```

**Option B: Or use pre-built AppImage (if available)**
```bash
chmod +x FreeBox_*.AppImage
./FreeBox_*.AppImage --headless
```

### Step 2: Load freebox.el in Emacs

**If using oremacs (already integrated):**
- `setup-freebox.el` is loaded automatically
- No extra steps needed

**Manual setup:**
```elisp
(add-to-list 'load-path "/path/to/freebox.el/")
(require 'freebox)
```

### Step 3: Use it

Open the main menu:
```
M-x freebox          — Main menu (transient)
  or  C-c v v
```

Menu options:
- **s** — Search videos
- **b** — Browse by category
- **S** — Change source

**Typical workflow:**
```
M-x freebox
  → s (search)
  → type keyword → pick result → pick episode → plays in mpv
```

Or use direct commands:
```
M-x freebox-search   — Search directly
M-x freebox-browse-category — Browse categories
M-x freebox-select-source   — Change source
```

---

## HTTP API Endpoints

FreeBox backend provides these REST endpoints:

| Method | Endpoint | Parameters | Response |
|--------|----------|------------|----------|
| GET | `/api/sources` | — | List of SourceBean |
| GET | `/api/search` | sourceKey, keyword | Search results (VodInfo list) |
| GET | `/api/categories` | sourceKey | Top-level categories |
| GET | `/api/category` | sourceKey, tid, page | Category content (paginated) |
| GET | `/api/detail` | sourceKey, vodId | Video details + episode list |
| GET | `/api/play` | sourceKey, playFlag, vodId | Playback URL |

All responses are JSON with structure: `{ "code": 200, "data": {...} }`

---

## Dependencies

| Package | Purpose |
|---------|---------|
| Emacs 28.1+ | Minimum version |
| `request.el` | HTTP client (async requests) |
| `transient` | Main menu UI (bundled with Emacs 28+) |
| `empv` | mpv integration for playback |
| Java 17+ | Required for FreeBox backend |
| `mpv` | Video player |

---

## Project Structure

```
freebox.el              — Main module (entry point)
freebox-http.el         — HTTP API client (async wrappers)
freebox-ui.el           — UI components (completing-read flows)
freebox-model.el        — Data model helpers (optional)
freebox-empv.el         — empv/mpv playback integration
freebox-commands.el     — M-x commands + transient menu
```

---

## Status

✅ Core features: search, browse, play
🔄 In development: FreeBox HTTP API in official repo
⏳ Future: History, favorites, subtitle support

---

## Contributing

To add the HTTP API to your FreeBox instance:

1. Copy `EmacsFrontendHandler.java` to FreeBox source
2. Register it in `FreeBoxHttpServerHolder`
3. Rebuild and test

See [FreeBox/src/main/java/io/knifer/freebox/net/http/handler/EmacsFrontendHandler.java](../../FreeBox/src/main/java/io/knifer/freebox/net/http/handler/EmacsFrontendHandler.java) for implementation details.
