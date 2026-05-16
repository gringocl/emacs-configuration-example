# CLAUDE.md

Personal Emacs configuration for Miles Starkenburg. Based on aaronjensen's
"emacs-configuration-example" teaching config and adapted.

## Source of truth: edit config.org, not config.el

This is a literate config. The flow is:

- `early-init.el` — hand-written, checked in. Loads before package init.
- `init.el` — hand-written, checked in. Bootstraps Elpaca + the
  `literate-config` package, defines the lazy-load hooks
  (`c/first-input-hook`, `c/first-file-hook`, `c/first-buffer-hook`), then
  calls `(literate-config-init)`.
- `config.org` — **the source of truth**. All package configuration lives
  in org-babel `emacs-lisp` src blocks here.
- `config.el` — **generated** by `literate-config` from `config.org` on
  startup. Gitignored. Never edit it; changes will be clobbered.

When asked to change a package's configuration, edit the corresponding
`#+begin_src emacs-lisp` block in `config.org`. Re-tangling happens
automatically at next Emacs start; there is no manual `M-x org-babel-tangle`
step in the workflow.

`init.el` IS edited by hand on rare occasions (elpaca bootstrap, hook
plumbing). It is not generated. Don't confuse `config.el` (generated,
ignored) with `init.el` (hand-written, committed).

## Files that must not be edited or committed

Already covered by `.gitignore`: `eln-cache/`, `elpaca/`, `config.el`,
`var/`, `etc/`.

NOT in `.gitignore` but should never be committed — leave them alone:

- `#init.el#` — Emacs autosave artifact. Delete locally if it bothers
  you, but do not stage it.
- `network-security.eld` — runtime cert state.
- `org-persist/**` — Org-mode runtime cache.
- `tree-sitter/*.dylib` — compiled tree-sitter grammars (the
  `tree-sitter/` directory itself is tracked, but the platform-specific
  dylibs are not).
- `auto-save-list/`, anything under `var/` or `etc/`.

If asked to commit, only stage files explicitly mentioned or that are
clearly real source edits (`config.org`, `init.el`, `early-init.el`,
`README.md`, `notes.org`, `Makefile`, `clean.sh`, `start.sh`,
`snippets/**`, `scripts/**`).

## Conventions inside config.org

- **`c/` prefix** for the owner's own functions, variables, faces, and
  hooks (`c/yank-buffer-path`, `c/first-input-hook`,
  `c/keyboard-quit-dwim`, etc). A few legacy prefixes (`ms/`, `rk/`,
  `+vc-`) appear in copied code; prefer `c/` for new additions.
- **`use-feature`** is a local macro defined in config.org — it's
  `use-package` with `:ensure nil`. Use it for built-in features
  (`dired`, `org`, `savehist`, `tab-bar`, …) and `use-package` for
  installed packages.
- **`:hook c/first-input` / `c/first-file` / `c/first-buffer`** — these
  are custom transient hooks defined in `init.el` that fire once after
  Emacs is idle. They're the preferred mechanism for lazy-loading
  non-essential packages (vertico, marginalia, flycheck, ws-butler, …).
  Use them instead of `:demand t` unless the package genuinely needs
  to load eagerly.
- **DISABLED headings** — section headings prefixed `DISABLED` (custom
  TODO keyword declared at the top of `config.org`) are blocks the
  owner has turned off but kept around for reference. `svg-tag-mode`
  renders them as a badge. Don't delete them; don't re-enable them
  without being asked.
- **Leader key** is configured via `leader-key` (aaronjensen's
  package). All bindings live in one big `leader-key-set` call near
  the top of config.org under "Leader Key", plus a few per-package
  additions scattered through their `use-package` blocks. Prefixes are
  declared with `leader-key-declare-prefix` (a/apps, b/buffers,
  f/files, g/git, p/projects, t/toggle, w/windows, …).
- **Evil** is on and is the editing model. New keybindings should
  respect evil states (`evil-define-key`, `:map evil-normal-state-map`,
  etc).
- Many packages are pulled from forks under `aaronjensen/*` on GitHub
  via Elpaca with `:protocol ssh`. Don't rewrite these to use HTTPS or
  upstream unless asked.

## Commit message style

Commit messages are written for a **future reader scanning the log as
an index**, not as a description of what the author just did. The
subject should let someone skim `git log --oneline` and quickly locate
the commit they need; the body (when present) is what they read once
they've found it.

Practical shape:

- Subject leads with *the subject of the change* — the package, file,
  setting, or area being changed — and tells the reader **what it
  does or what it now supports**, not just that something happened
  there. `browse-at-remote supports selected lines for markdown
  files`, not `browse-at-remote markdown URLs`.
- No filler verbs like "is added" or "is updated". Just name the
  thing: `c/slugify-title`, not `c/slugify-title is added`. The diff
  shows it's new.
- Short. Scannable. Locatability is the goal.
- **No body by default.** The diff is the description. Only write a
  body when the *why* is genuinely unknowable from the diff (rare).
- **No `Co-Authored-By` trailer.**
- No conventional-commits prefixes, no emojis, no "feat:" / "fix:"
  tags — those clutter the index without helping the scan.

Examples from history (note how each one names the subject up front so
it scans):

```
Config is updated
Evil is elaborated
gptel is disabled
Snippets are elaborated
Elpaca version is increased from 0.9 to 0.11
Quotes are not required in bind
```

These happen to be passive-voice phrasings, but the rule is **subject-led
and indexable**, not "use passive voice." Don't mimic the grammar; mimic
the orientation.

## Spell-checking

Uses **Jinx** (not flyspell) — see the "Spell Checking" section in
`config.org`. Configured for English + Spanish (`jinx-languages
"en_US es"`); a word passes if either dictionary accepts it. Jinx
splits camelCase/snake_case identifiers in `prog-mode` and checks each
fragment separately. Requires `brew install enchant` system-side
(uses existing aspell dictionaries as backend).

## Useful local utilities to know about

Defined in `config.org` (search by name there for the full definition):

- `c/yank-buffer-path`, `c/yank-buffer-path-relative-to-project`
- `c/copy-visited-file`, `c/delete-this-file`
- `c/search-project-for-symbol` — ripgrep-via-consult on symbol at point
- `c/consult-ripgrep-in-directory`
- `c/project-name`, `c/project-root`
- `c/insert-log-date` — date-stamp insertion (lives in `notes.org`, not
  config.org; bound to `C-x C-d`)
- `c/keyboard-quit-dwim` — replaces `C-g` with smarter behavior

## Template-markup mode

The last big section of `config.org` ("Template Markup Mode") defines
an in-tree major mode (`template-markup-mode`) for editing Neuron
`template-markup` / `docx-markup` files (`.tmpl`, `.template`, and
some `.txt` files matched by content sniffing). If asked to touch
markup highlighting, that's where it lives.

## Things that are NOT here

- No `lisp/` directory or external `.el` modules. Everything is in
  `config.org` + `init.el` + `early-init.el`.
- No byte-compilation of user code (`no-byte-compile: t` is set on
  `init.el` and `early-init.el`).
- No CI, no tests.

## Notes file

`notes.org` is a personal scratchpad of TODOs and utility-function
drafts. Some functions get promoted into `config.org` later. Treat it
as an inbox; don't reorganize it without being asked.
