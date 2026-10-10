# keel

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
- **Dynamic effect capture.** While a body runs, `add-hook`, `advice-add`,
  `define-key`/`keymap-set`, and `customize-set-variable` are advised, and
  the body's `setq` targets carry variable watchers. Effects from loops,
  helpers and called functions are captured, with old values, not only the
  forms visible at top level.
- **Transactional units.** A body that signals an error has its captured
  effects retracted, so a half-applied unit does not leave half its state
  behind.
- **Effect-aware reload.** `keel-reload` diffs declarations against the
  previous load, retracts the captured effects of changed units, and
  re-applies only those. Unchanged units are skipped; removed units are
  retracted.
- **Checked-lazy.** A unit with `:defer FEATURE` is preflighted and linted at
  startup like every other unit, but its body runs on `with-eval-after-load`.
  Ordering among deferred units follows the graph. Errors surface at startup;
  startup stays fast.
- **Interactive tooling.** `keel-explain` shows a unit's place in the plan,
  why it was skipped, what it cost, and jumps to its source. `keel-list-units`
  is a `tabulated-list` of every unit. `keel-rerun-unit` re-applies one unit.
  Timings come from `benchmark-run` per unit.
- **`keel check` is `emacs --batch`.** The same planning and lint code,
  without execution. Exit codes make a dotfiles repo CI-able. No binary to
  distribute.

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
  keel-package.el   package-vc bridge, keel.lock, upgrade/prune
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
`config.el`, plans, executes, and shows the report if anything failed.

```sh
emacs --batch -l ~/path/to/keel/lisp/keel.el -f keel-check-batch
```

validates the config without executing it. Exit `0` clean, `1` findings,
`2` harness error.

## Requirements

Emacs 31 (uses `package-vc-install`, `read-positioning-symbols`,
`add-variable-watcher`).

## Status

Under construction. Milestones, in order:

1. README, test harness
2. `keel-plan`: declarations, graph, preflight, plan
3. `keel-exec`: executor, fail-forward, timing
4. `keel-static`: macro-time analysis, byte-compiler lint
5. `keel-effects`: capture, transactional units, retract
6. `keel-config`: tangle, env, boot, reload
7. `keel-report`: report buffer, explain, list, rerun
8. `keel-check`: batch validation
9. `keel-package`: package-vc bridge, lockfile, upgrade/prune
10. `:defer`, end-to-end test in a throwaway home

## Lineage

emacs-backbone (Gleam) → emacs-hypervisor (Elle) → keel (Emacs Lisp). The
model survived; the hosts did not.
