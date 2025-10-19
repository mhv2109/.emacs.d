# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration repository (`~/.emacs.d`) using a single-file `init.el` approach with `use-package` for declarative package management. The configuration emphasizes a literate, well-documented setup for software development across multiple languages (Go, Python, TypeScript, Java, Lisp) with integrated AI tooling and org-mode for knowledge management.

## Architecture

### Configuration Structure

- **Single-file configuration**: All configuration lives in `init.el` (1073 lines)
- **Package management**: Uses `use-package` with `:ensure t` set globally for automatic package installation from MELPA, MELPA-stable, and GNU ELPA
- **Lexical scoping**: Configuration uses `lexical-binding: t` throughout
- **Conditional loading**: Extensive use of `:if`, `:after`, and `:hook` to load packages conditionally and lazily

### Key Design Patterns

1. **Use-package declarations**: Each package follows this pattern:
   ```elisp
   (use-package package-name
     :custom (variable value)
     :config (configuration-code)
     :hook (mode . function)
     :bind (keybindings))
   ```

2. **Helper macros**: Custom macros like `add-server-program-if-found` check for executables before adding LSP servers

3. **Conditional configuration**: AI backends (Ollama, Anthropic, Copilot) are configured conditionally based on environment variables or executable presence

4. **Built-in package handling**: Uses `:ensure nil` for built-in Emacs packages

### Major Subsystems

#### LSP + Debugging (eglot + dape)
- **eglot** (built-in): LSP client, configured to auto-start for prog-mode/org-mode/markdown-mode
- **dape**: Debug Adapter Protocol client with custom configurations for Go (dlv), TypeScript/JavaScript (Jest), and Java (jdtls)
- Auto-formatting on save for Go and Terraform via `eglot-code-action-organize-imports` and `eglot-format-buffer`

#### AI Integration (gptel + MCP)
- **gptel**: LLM chat client with multiple backends (Ollama, Anthropic Claude, GitHub Copilot)
- **mcp.el** (Emacs 30.1+): Model Context Protocol integration with containerized servers:
  - General: fetch, duckduckgo, sequential-thinking, markitdown, filesystem
  - Programming: serena (coding agent), context7 (library docs), AWS/Cloudflare/Terraform docs
- **gptel-commit**: AI-generated commit messages
- Interactive tool selection via `gptel-mcp-connect`

#### Org-mode + Knowledge Management
- **org-roam**: Zettelkasten-style note-taking with custom capture templates for projects, areas, resources, books, and websites
- **Directory structure**: Flat organization under `org-directory` with subdirectories: `dailies/`, `resources/`, `projects/`, `areas/`, `data/` (attachments), `archived/`
- **deft**: Full-text search across org files
- **org-remark**: Highlighting and annotations for org, Info, EWW, and EPUB files
- **org-babel**: Enabled for Python, shell, and Go code execution

#### Git Workflow
- **magit**: Primary Git interface (same-window display by default)
- **forge**: GitHub/GitLab integration
- **git-link**: Generate permalinks to current position in repository
- **smerge-mode**: Built-in merge conflict resolution

#### Language-Specific Tooling

**Go**:
- go-mode / go-ts-mode (treesitter, pinned to v0.19.1 due to Linux compatibility)
- flymake-golangci: golangci-lint integration
- gotest: Quick test execution
- eglot + gopls (auto-configured)
- dape configs: `dlv-current-file` (debug file) and `dlv-unit-test` (debug test at point)

**Python**:
- Built-in python.el with custom ipython integration (loads `autoload.ipy` on startup)
- pyvenv + pyvenv-auto: Virtual environment management
- Python shell interpreter auto-switches to project-local ipython on venv activation

**TypeScript/JavaScript**:
- typescript-mode
- dape config: `jest` for debugging Jest unit tests

**Java**:
- eglot + jdtls with Lombok support via javaagent
- dape integration with java-debug-server-plugin

**Others**:
- yaml-mode / yaml-ts-mode (tab-width: 2)
- terraform-mode
- dockerfile-mode, fish-mode, protobuf-mode, lua-mode, markdown-mode
- paredit for Lisp editing (emacs-lisp, common lisp, clojure)

#### UI/UX Enhancements
- **Completion**: corfu (manual, TAB-triggered), hotfuzz (fuzzy matching)
- **Minibuffer**: ivy + counsel + marginalia
- **Context actions**: embark (integrates with which-key for discoverable keybindings)
- **Navigation**: projectile (C-c p prefix), neotree integration, swiper for search
- **Scrolling**: ultra-scroll for smooth pixel-perfect scrolling
- **Terminal**: vterm (libvterm-based)
- **Theme**: doom-gruvbox

## Development Commands

### Testing Emacs Lisp Changes

Restart Emacs to test configuration changes:
```bash
emacs  # GUI
emacs -nw  # Terminal
```

Byte-compile init.el to check for errors (optional):
```bash
emacs --batch -f batch-byte-compile init.el
```

Profile startup time:
```elisp
;; Load profile-dotemacs.el and run:
M-x profile-dotemacs
```

### Package Management

Refresh package archives and update packages:
```elisp
M-x package-refresh-contents
M-x auto-package-update-now
```

Install a new package (use-package will auto-install on next restart if added to init.el):
```elisp
M-x package-install RET package-name
```

### Language-Specific Testing

**Go tests**:
```elisp
M-x gotest-run              ;; Run all tests in current package
M-x gotest-current-test     ;; Run test at point
M-x gotest-current-file     ;; Run tests in current file
```

**Python shell** (with ipython if available):
```elisp
M-x run-python              ;; Start Python REPL
C-c C-c                     ;; Send buffer/region to REPL
```

**Org-babel evaluation**:
```elisp
C-c C-c                     ;; Execute code block at point
C-c C-v b                   ;; Execute entire buffer
```

## Key Conventions

### Coding Style (from .github/copilot-instructions.md)
- Write idiomatic elisp that is **correct**, **clear**, and **efficient**, in that order
- Prefer lexical scoping (`lexical-binding: t`)
- Keep documentation and comments up-to-date using concise, clear docstrings

### File Organization
- All configuration in `init.el` (do not create separate module files)
- Secrets stored in `~/.emacs.d/secrets.el.gpg` (GPG-encrypted, loaded at startup)
- Backups in `~/.emacs.d/backups/`, autosaves in `~/.emacs.d/autosaves/`
- ELPA packages in `~/.emacs.d/elpa/`, native-comp cache in `~/.emacs.d/eln-cache/`

### Custom Functions
Helper functions are defined inline within init.el:
- `copy-buffer-file-path`: Copy current buffer's file path to kill ring
- `copy-projectile-project-path`: Copy project root to kill ring
- `set-python-shell-interpreter-ipython`: Configure ipython as Python shell
- `toggle-window-split`: Switch between horizontal and vertical splits
- `load-if-exists`: Conditionally load files if they exist

### Package Installation Patterns
**Standard packages** (from MELPA):
```elisp
(use-package package-name)  ;; :ensure t is set globally
```

**Built-in packages**:
```elisp
(use-package package-name
  :ensure nil)
```

**VC packages** (installed from Git):
```elisp
(use-package package-name
  :vc (:url "https://github.com/user/repo.git" :rev :newest))
```

### Treesitter
- Requires Emacs with treesitter support (`treesit-available-p`)
- `treesit-auto` handles automatic installation of grammars
- Custom recipes for Go (pinned to v0.19.1) and fish
- Treesitter modes automatically remap from standard modes (e.g., go-mode → go-ts-mode)

### MCP Server Management
MCP servers run in Docker containers or via npx/uvx. To modify MCP configuration:
1. Edit `mcp-hub-servers` alist in init.el
2. Restart Emacs or `M-x mcp-hub-restart-server RET server-name`
3. Use `M-x gptel-mcp-connect` to select tools interactively in gptel buffers

Note: The `serena` MCP server must use `uvx` (not Docker) to access filesystem properly.

## Common Customizations

### Adding a New Language Mode
```elisp
(use-package language-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ext\\'" . language-mode))
  :hook
  (language-mode . eglot-ensure))  ;; Enable LSP
```

### Adding a New LSP Server
```elisp
(add-server-program-if-found "language-server-executable" t
  `(language-mode ,command "args"))
```

### Adding Keybindings
Global:
```elisp
(global-set-key (kbd "C-c x") 'command-name)
```

Mode-specific (within use-package):
```elisp
(use-package package-name
  :bind
  (("C-c x" . command-name)
   :map mode-specific-map
   ("C-c y" . other-command)))
```

### Configuring AI Backends
Set environment variables before starting Emacs:
```bash
export ANTHROPIC_API_KEY="your-key-here"
emacs
```

Or add to `~/.emacs.d/secrets.el.gpg`:
```elisp
(setenv "ANTHROPIC_API_KEY" "your-key-here")
```

## Important Notes

- **Performance**: `gcmh` (GC Magic Hack) is enabled to reduce GC pauses
- **Warning suppression**: Warnings are suppressed (`warning-minimum-level :emergency`) - be cautious when debugging
- **Max eval depth**: Set to 10000 to handle deep Java dependency trees
- **Trailing whitespace**: Automatically deleted on save for all files
- **Auto-revert**: Enabled globally - buffers automatically refresh when files change on disk
- **Window splitting**: Disabled by default for sensible full-window displays (magit, dired, etc.)
- **exec-path-from-shell**: Required on macOS/GUI environments to inherit shell PATH
