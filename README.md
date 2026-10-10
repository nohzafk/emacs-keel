# emacs-keel

Deterministic, dependency-aware Emacs configuration, in pure Emacs Lisp.

A keel is the backbone of a boat. Its job is not to move the boat; it is to
keep it upright and on course. keel does the same for an Emacs config: it
turns a pile of order-sensitive forms into a declared dependency graph that is
planned before anything runs, executed eagerly in a deterministic order, and
reported with every failure traced to its source line.

keel is a foundation for your own config, not a distribution. You keep
`config.org` (or `config.el`); keel gives you two macros and the machinery
around them.

## The model

```elisp
(package! magit)
(package! transient :repo "magit/transient" :branch "main")

(config-unit! magit-ui
  :requires (magit)
  :executable (git)
  :config
  (keymap-set global-map "C-x g" #'magit-status))

(config-unit! project-hooks
  :after (magit-ui)
  :config
  (add-hook 'magit-mode-hook
            (lambda () (setq-local truncate-lines t))))
```

| Macro | Purpose |
|---|---|
| `package!` | Declare a package and its dependencies |
| `config-unit!` | Declare a named block of configuration and what it needs |

`config-unit!` options:

| Option | Meaning |
|---|---|
| `:after` | Run only after these units have completed |
| `:requires` | Load these features before the body runs |
| `:env` | Skip the unit if any of these environment variables is unset |
| `:executable` | Skip the unit if any of these binaries is missing from `PATH` |
| `:defer` | Preflight now, run the body when this feature loads (see *Checked-lazy*) |
| `:config` | The body |

## Why

Most Emacs configs are scripts whose correctness depends on evaluation order,
and whose errors hide behind lazy loading until the feature is first used
hours later. keel takes the opposite trade:

- **Plan before execute.** Declarations build a graph. The graph is validated
  (duplicate names, missing references, cycles), preflighted (env vars,
  executables, features), and topologically ordered, before a single body
  runs. Planning is pure and runs the same in batch, so `keel check` in CI
  and startup agree.
- **Eager and deterministic.** Units run in plan order at startup. A broken
  unit fails at startup, with the context still fresh.
- **Fail forward.** A failed package or unit skips its dependents and nothing
  else. The report says what failed, what was skipped, and why.
- **Provenance everywhere.** Every declaration knows its file, Org heading and
  line. Every failure, lint finding and skipped unit links back to it.

## What living inside Emacs buys

keel is the fourth implementation of this idea; the previous three
([emacs-backbone](https://github.com/nohzafk/emacs-backbone) in Gleam,
emacs-hypervisor in Elle, a Bend prototype) each ran the planner as an
external process speaking RPC to Emacs over stdio. Three languages, one shape,
one lesson: the process boundary was the cost, not the language. An external
planner only ever sees the config as data. In-process, keel has the reader,
the symbol table, the byte-compiler and the runtime, and uses them:

- **Static analysis at macro time.** `config-unit!` walks its body when it
  expands: it collects `setq` targets, hook/advice/keybinding forms and
  `foo-mode` references. From these it infers missing `:requires`, flags
  unused ones, and knows which globals to watch before the body runs.
- **The byte-compiler is the linter.** Each unit body is byte-compiled and
  its warnings (undefined function, free variable, wrong arity) are reported
  with the unit's provenance. No hand-written lint.
- **Dynamic effect capture.** While a body runs, `add-hook`, `advice-add`
  and `define-key` (which `keymap-set`, `global-set-key` and friends call)
  are advised, and the globals the static pass saw assigned carry variable
  watchers. Effects from loops, helpers and called functions are captured,
  with old values, not only the forms visible at top level. What a library
  does while it loads inside the body is its own and is not captured;
  neither is a hook or advice that was already installed.
- **Transactional units.** A body that signals an error has its captured
  effects retracted, so a half-applied unit does not leave half its state
  behind.
- **Effect-aware reload.** `keel-reload` diffs declarations against the
  previous load, retracts the captured effects of changed and removed units
  and of every unit `:after` them, newest first, and re-applies only those.
  Everything else is left alone.
- **Checked-lazy.** A unit with `:defer FEATURE` is preflighted and linted at
  startup like every other unit, but its body runs when the feature loads.
  A unit `:after` a deferred unit waits for it too, and a deferred unit that
  fails blocks its dependents like any other. Errors surface at startup;
  startup stays fast.
- **Interactive tooling.** `keel-explain` shows a unit's place in the plan,
  why it was skipped, what it cost, and jumps to its source. `keel-list-units`
  is a `tabulated-list` of every unit. `keel-rerun-unit` re-applies one unit.
  Every unit is timed.
- **`keel check` is `emacs --batch`.** The same planning and lint code,
  with no unit body executed. Exit codes make a dotfiles repo CI-able. No
  binary to distribute. Top-level forms that are not declarations are still
  evaluated, and lint macroexpands each body, so `eval-when-compile` runs.

## What it does not do

Say it on day one so nobody has to find out:

- **No concurrency.** Emacs evaluates one form at a time. keel orders work;
  it does not parallelise it. The only parallel work is git clones.
- **No complete revocation.** Elisp effects are not reified. keel captures
  hooks, advice, keybindings, customised variables and the globals its static
  pass can see. Everything else (`add-to-list`, mode toggles, `defun`
  redefinitions, timers, faces, `auto-mode-alist` surgery) re-evaluates on
  reload as it always has. Restart is the only complete revert, and with
  eager deterministic boot it is a fast one.

## Layout

```text
lisp/
  keel-plan.el      declarations, graph, preflight, plan  (pure, no side effects)
  keel-exec.el      executor: plan -> results, fail-forward
  keel-static.el    macro-time body analysis, byte-compiler lint
  keel-effects.el   effect registry, dynamic capture, retract
  keel-config.el    config.org tangling, env file, keel-boot, keel-reload
  keel-report.el    report buffer, keel-explain, keel-list-units
  keel-check.el     batch validation entry point
  keel-package.el   clone, link into user-lisp-directory, keel.lock, upgrade/prune
  keel.el           entry point
test/               ERT, run with `make test`
scripts/            keel-check wrapper, e2e harness
```

Planning is a pure function from declarations to a plan. The executor walks
the plan. `keel check` runs the planner and stops. That separation, kept as a
function boundary rather than a process boundary, is what the three previous
implementations were really about.

## Usage

```elisp
;; init.el
(add-to-list 'load-path "~/path/to/keel/lisp")
(require 'keel)
(keel-boot)
```

`keel-boot` loads `$XDG_CONFIG_HOME/keel/config.org` (tangled) or
`config.el`, injects the optional `env` file, plans, installs packages,
runs units, and shows the report if anything failed.

```text
~/.config/keel/
├── config.org   literate config (preferred); or config.el
├── env          optional: Lisp list of "KEY=VALUE" strings, injected at boot
└── keel.lock    installed package revisions, written by keel; commit it
```

```sh
scripts/keel-check [--strict] [--format sexp] [--config DIR]
```

validates the config without running any unit body: tangle, plan,
preflight, static lint, byte-compiler lint. Exit `0` clean, `1` a
declaration that failed to load or is invalid (or a lint warning with
`--strict`), `2` harness error. Units
skipped by preflight are reported but do not fail the check: they depend
on the machine, not the config.

### Commands

| Command | Does |
|---|---|
| `keel-reload` | re-read the config; retract and re-apply changed units only |
| `keel-report` | the report buffer: problems, packages, units, timings, lint; links jump to source |
| `keel-explain` | one unit: declaration, verdict, dependents, captured effects, lint, body |
| `keel-list-units` | every unit in a table; `RET` explains, `j` jumps to source, `r` reruns |
| `keel-rerun-unit` | retract one unit's effects and run its body again |
| `keel-upgrade-package`, `keel-upgrade-all-packages` | ignore the lock, honour the declaration, report the revision delta |
| `keel-rebuild-package` | drop the clone and cache, re-clone, re-run `:build`, re-link |
| `keel-prune-packages` | list installed-but-undeclared packages and remove them after confirmation |

`KEEL_UPGRADE_PACKAGES=name1,name2` (or `all`) and `KEEL_REBUILD_PACKAGES`
do the same at boot.

## Packages

`package!` options:

| Option | Meaning |
|---|---|
| `:repo` | git repository, `"owner/name"` or a full URL |
| `:host` | `"github"` (default), `"gitlab"`, `"codeberg"`, `"sourcehut"` |
| `:branch`, `:tag`, `:ref` | what to track or pin |
| `:local` | a checkout on disk; linked directly, never cloned or locked |
| `:lisp-dir` | subdirectory holding the `.el` files |
| `:deps` | packages that must be installed first |
| `:submodules` | run `git submodule update --init --recursive` in the clone |
| `:build` | shell command(s) run in the clone before activation |

A bare `(package! consult)` installs from GNU ELPA, NonGNU ELPA or MELPA
with `package-install`. Anything with `:repo`, `:host` or `:local` goes
through Emacs 31's User Lisp Directory: the clone (or the `:local` path)
is linked into `user-lisp-directory` and `prepare-user-lisp` byte-compiles
it and scrapes its autoloads. keel treats `<user-lisp-directory>/<name>` as
its own for every declared package: rebuild and prune delete it. Archive
packages go to `<user-emacs-directory>/keel/packages/`, which keel sets as
`package-user-dir` at boot; packages installed by hand into `elpa/` are
not seen. `package-vc-install-from-checkout` is
obsolete in 31 and is not used. Clones are fetched in parallel before the
plan runs; adoption is per package, in plan order.

`keel.lock` records every installed revision. Precedence at install: a
declared `:ref`/`:tag` wins and rewrites the lock; else the locked
revision; else the branch or default HEAD, whose result is locked. A fresh
machine with `config.org` and `keel.lock` reproduces the revisions that
wrote the lock.

## Requirements

Emacs 31 (uses `package-vc-install`, `read-positioning-symbols`,
`add-variable-watcher`).

## Development

```sh
make test      # ERT in batch
make compile   # byte-compile with warnings as errors
scripts/e2e    # boot a throwaway home, then keel-check it
```

## Status

Every module in the layout exists and is tested in batch. Network paths
(real clones from a forge, archive installs) have been exercised by hand,
not by the test suite, which uses local `file://` repositories.

### Known gaps

- **Rebuild and upgrade purge before they fetch.** `keel-rebuild-package`,
  `keel-upgrade-package` and the `KEEL_*_PACKAGES` env vars delete the clone
  and the link first, then clone again. If the fetch fails, the previously
  working package is gone until the next successful fetch; only `keel.lock`
  still names the old revision. The fix is clone-to-temp-then-swap.
- **A changed pin on an installed package is a no-op at boot.** Editing
  `:ref`, `:tag`, `:branch` or `:repo` of a package that is already linked
  changes nothing and warns about nothing. `keel-rebuild-package` applies the
  new declaration. Comparing the declaration to the lock entry at install
  time would catch it.
- **`keel-rerun-unit` skips preflight.** It runs the body even if the planner
  had skipped the unit (missing binary, invalid declaration), and a rerun of
  a `:defer` unit whose feature is already loaded reports `deferred` instead
  of the body's result.
- **Deferred registrations accumulate.** Each boot or reload of a `:defer`
  unit adds one `with-eval-after-load` closure that is never removed. They
  are inert once the pending entry is gone, but a later re-`load` of the
  feature runs them again.
- **`define-key` from C is not captured.** The advice sees Lisp callers
  only. Natively compiled callers are untested here.
- **`scripts/e2e` touches the real `user-emacs-directory`.** It runs `-Q`
  with a temp config dir but not a temp Emacs home, so an empty
  `<user-emacs-directory>/keel/` appears after a run.
- **Package names are trusted.** A name containing `/` or `..` becomes a
  path under the state root without validation.

## Lineage

emacs-backbone (Gleam) → emacs-hypervisor (Elle) → keel (Emacs Lisp). The
model survived; the hosts did not.
