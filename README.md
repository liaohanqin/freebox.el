# freebox.el

Emacs client for [FreeBox](https://github.com/kknifer7/FreeBox).
Search and stream video sources directly inside Emacs using empv/mpv for playback.
Features poster preview, thumbnail gallery, menu state persistence, and resume navigation.

**Requires**: FreeBox backend with HTTP REST API (see below).

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
   - Pretty-Hydra main menu for easy navigation
   - Poster preview and thumbnail gallery (graphical Emacs)
   - Menu state persistence across sessions

---

## Setup

### Step 1: Deploy FreeBox with HTTP API

The default FreeBox release does **not** include the Emacs HTTP API endpoints.
You need to:

**Option A: Build from patched source**
```bash
cd /path/to/FreeBox
# Apply patch that adds EmacsFrontendHandler
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
M-x freebox          — Main menu (Pretty-Hydra)
  or  C-c v v
```

---

## Main Menu (Pretty-Hydra)

| Key | Command | Description |
|-----|---------|-------------|
| `x` | Select client | Choose client config (video source JSON) |
| `y` | Select source | Choose a source within client |
| `z` | Select category | Choose a category within source |
| `s` | Search videos | Full-text search |
| `v` | Resume last pos | Resume from last navigation position |
| `r` | Start server | Start FreeBox backend |
| `k` | Stop server | Stop managed backend |
| `q` | Quit | Close menu |

---

## Typical Workflow

```
M-x freebox
  → x (select client, first time only)
  → z (select category) → pick a video → poster preview → RET (play)
```

1. **Select client** — choose which video source config to use
2. **Select source** — pick a source within the client
3. **Browse category** — select category → browse videos
   - Items with posters show `[*]` indicator
   - Select `-- 查看海报集 --` to open thumbnail gallery
4. **VOD detail** — poster preview buffer with metadata
   - `RET` or `p` — enter episode selection
   - `q` — go back
5. **Episode selection** — pick play source → pick episode → plays in mpv

---

## Poster Preview

When entering VOD detail, a poster preview buffer (`*freebox-poster*`) is shown with:
- Title, rating/year, actors
- Large poster image (async loaded, auto-scaled to window)
- Description

Falls back to text-only in terminal Emacs or when poster URL is absent.

---

## Poster Gallery

Select `-- 查看海报集 --` from the category page menu to open a thumbnail grid (`*freebox-gallery*`):

| Key | Action |
|-----|--------|
| `j` | Next item |
| `k` | Previous item |
| `n` | Next page |
| `p` | Previous page |
| `RET` | Open VOD detail |
| `q` | Return to list |

Thumbnails are loaded from local cache. The gallery option is only available in graphical Emacs.

---

## Image Cache

Poster images are cached in `~/.freebox/cache/posters/` with 30-day expiry.

| Command | Description |
|---------|-------------|
| `M-x freebox-image-clear-cache` | Delete all cached posters |
| `M-x freebox-image-cleanup-expired` | Remove expired posters |

---

## HTTP API Endpoints

FreeBox backend provides these REST endpoints:

| Method | Endpoint | Parameters | Response |
|--------|----------|------------|----------|
| GET | `/api/clients` | — | Client config list |
| GET | `/api/sources` | clientId? | List of SourceBean |
| GET | `/api/search` | sourceKey, keyword, clientId? | Search results |
| GET | `/api/categories` | sourceKey, clientId? | Top-level categories |
| GET | `/api/category` | sourceKey, tid, page?, clientId? | Category content (paginated) |
| GET | `/api/detail` | sourceKey, vodId, clientId? | Video details + episodes |
| GET | `/api/play` | sourceKey, playFlag, vodId, clientId? | Playback URL |

All responses: `{ "code": 200, "data": {...} }`

---

## Dependencies

| Package | Purpose |
|---------|---------|
| Emacs 28.1+ | Minimum version (30+ for gallery image scaling) |
| `request.el` | HTTP client (async requests) |
| `pretty-hydra` | Main menu UI |
| `empv` | mpv integration for playback |
| Java 17+ | Required for FreeBox backend |
| `mpv` | Video player |

---

## Project Structure

```
freebox.el              — Main module (entry point, requires all others)
freebox-http.el         — HTTP API client (async wrappers, server management)
freebox-ui.el           — UI components (completing-read flows, navigation)
freebox-image.el        — Image cache, poster preview buffer, gallery mode
freebox-commands.el     — M-x commands + Pretty-Hydra menu
freebox-persist.el      — Menu state persistence (client/source/category/v-cursor)
freebox-model.el        — Data model accessors
freebox-empv.el         — empv/mpv playback integration
```

---

## Status

- ✅ Core: search, browse, play
- ✅ Poster preview (VOD detail large image)
- ✅ Gallery view (thumbnail grid with pagination)
- ✅ Menu persistence and resume navigation
- ✅ Auto-start backend server
- ⏳ Future: search history, favorites, subtitle support
