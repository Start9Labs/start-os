import type { Effects } from '../Effects'
import { FullProgress, Progress, ProgressUnits } from '../osBindings'

/**
 * The effect a root tracker pushes its snapshot through when `sync()` is
 * called — `setInitProgress` for init, `setBackupProgress` for backup. The
 * harness bakes this in; service code never calls the effect directly.
 */
export type ProgressSink = (
  effects: Effects,
  progress: Progress,
) => Promise<unknown>

/**
 * Mirror of the core Rust `FullProgressTracker`. Use this when a service's
 * init / backup / restore procedure has internal phases it wants to surface
 * to the host's progress UI.
 *
 * Service code does not call the progress effect directly. The init and
 * backup harnesses build a root tracker (with the right effect baked in) and
 * hand each handler its own tracker. Add phases, update them, and call
 * `tracker.sync(effects)` — that walks up to the root and reports the whole
 * tree via the host effect. On the wire a snapshot lands as
 * `Progress::Nested(FullProgress)`.
 *
 * Phases can be nested: `addNestedPhase` returns a child tracker whose
 * snapshot the parent folds in as a `Progress::Nested(...)` value, and whose
 * `sync()` bubbles up to this tracker.
 */
export class FullProgressTracker {
  private phases: Array<{
    name: string
    contribution: number | null
    value: () => Progress
  }> = []
  private completed = false
  private parent?: FullProgressTracker

  constructor(private readonly pushEffect?: ProgressSink) {}

  addPhase(name: string, contribution: number | null = 1): PhaseHandle {
    const handle = new PhaseHandle()
    this.phases.push({ name, contribution, value: () => handle.snapshot() })
    return handle
  }

  addNestedPhase(
    name: string,
    contribution: number | null = 1,
  ): FullProgressTracker {
    const child = new FullProgressTracker()
    child.parent = this
    this.phases.push({ name, contribution, value: () => child.snapshot() })
    return child
  }

  /** Mark the overall progress as complete. Does not mutate individual phases. */
  complete(): void {
    this.completed = true
  }

  /** Drop all phases and clear completion — used to start a fresh pass when a handler re-runs. */
  reset(): void {
    this.phases = []
    this.completed = false
  }

  /**
   * Report the current progress to the host. Walks up to the root tracker and
   * pushes the root snapshot through the effect baked in at construction
   * (`setInitProgress` / `setBackupProgress`). No-op for a root with no sink.
   * Errors are swallowed — progress reporting is best-effort and a no-op
   * outside the relevant transition.
   */
  async sync(effects: Effects): Promise<void> {
    if (this.parent) return this.parent.sync(effects)
    if (this.pushEffect)
      await this.pushEffect(effects, this.snapshot()).catch(() => null)
  }

  snapshot(): FullProgress {
    return {
      overall: this.completed ? true : this.computeOverall(),
      phases: this.phases.map(p => ({
        name: p.name,
        progress: p.value(),
      })),
    }
  }

  private computeOverall(): Progress {
    const weighted = this.phases.filter(p => p.contribution !== null)
    if (weighted.length === 0) return null
    const total = weighted.reduce(
      (acc, p) => acc + (p.contribution as number),
      0,
    )
    if (total <= 0) return null
    let done = 0
    let anyStarted = false
    for (const p of weighted) {
      const v = p.value()
      done += progressRatio(v) * (p.contribution as number)
      if (v !== null) anyStarted = true
    }
    if (!anyStarted) return null
    return { done: Math.floor(done), total, units: null }
  }
}

/** Handle returned by `FullProgressTracker.addPhase`. Drives one leaf phase. */
export class PhaseHandle {
  private state: Progress = null

  start(): void {
    if (this.state === null) this.state = false
  }

  setDone(done: number): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done, total: null, units: null }
      return
    }
    const clamped =
      this.state.total !== null ? Math.min(done, this.state.total) : done
    this.state = { ...this.state, done: clamped }
  }

  setTotal(total: number): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done: 0, total, units: null }
      return
    }
    this.state = { ...this.state, total }
  }

  addTotal(total: number): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state) ||
      this.state.total === null
    ) {
      this.setTotal(total)
      return
    }
    this.state = { ...this.state, total: this.state.total + total }
  }

  setUnits(units: ProgressUnits | null): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done: 0, total: null, units }
      return
    }
    this.state = { ...this.state, units }
  }

  complete(): void {
    this.state = true
  }

  /** Replace this phase's value wholesale — accepts any `Progress`, including a nested `FullProgress`. */
  setRaw(value: Progress): void {
    this.state = value
  }

  snapshot(): Progress {
    return this.state
  }
}

function isFullProgress(p: Progress): p is FullProgress {
  return p !== null && typeof p === 'object' && 'overall' in p
}

/** Scalar `Progress` — every variant except `Nested(FullProgress)`. */
export type LeafProgress = Exclude<Progress, FullProgress>

/** Walk through any `Nested` layers and return the scalar `Progress`. */
export function leafProgress(p: Progress): LeafProgress {
  let cur = p
  while (isFullProgress(cur)) cur = cur.overall
  return cur as LeafProgress
}

function progressRatio(p: Progress): number {
  const leaf = leafProgress(p)
  if (leaf === true) return 1
  if (leaf === false || leaf === null) return 0
  if (leaf.total === null || leaf.total <= 0) return 0
  return Math.min(1, leaf.done / leaf.total)
}
