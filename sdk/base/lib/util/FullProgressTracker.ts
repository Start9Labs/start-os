import {
  FullProgress,
  PhaseProgress,
  Progress,
  ProgressUnits,
} from '../osBindings'

/**
 * Mirror of the core Rust `FullProgressTracker`. Use this when a service's
 * backup (or restore) procedure has internal phases it wants to surface to
 * the host's progress UI.
 *
 * Wire it up by `snapshot()`ing on demand and sending the result via
 * `effects.setBackupProgress({ progress: tracker.snapshot() })`.
 *
 * Phases can be nested: `addNestedPhase` returns a child tracker that
 * the parent's snapshot folds in as `PhaseProgress::Nested(...)`.
 */
export class FullProgressTracker {
  private phases: Array<{
    name: string
    contribution: number | null
    value: () => PhaseProgress
  }> = []
  private completed = false

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
    this.phases.push({ name, contribution, value: () => child.snapshot() })
    return child
  }

  /** Mark the overall progress as complete. Does not mutate individual phases. */
  complete(): void {
    this.completed = true
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
    const total = weighted.reduce((acc, p) => acc + (p.contribution as number), 0)
    if (total <= 0) return null
    let done = 0
    let anyStarted = false
    for (const p of weighted) {
      const v = p.value()
      const ratio = phaseRatio(v)
      done += ratio * (p.contribution as number)
      if (!isNotStarted(v)) anyStarted = true
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
    if (this.state === null || this.state === false) {
      this.state = { done, total: null, units: null }
      return
    }
    const clamped =
      this.state.total !== null ? Math.min(done, this.state.total) : done
    this.state = { ...this.state, done: clamped }
  }

  setTotal(total: number): void {
    if (this.state === true) return
    if (this.state === null || this.state === false) {
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
      this.state.total === null
    ) {
      this.setTotal(total)
      return
    }
    this.state = { ...this.state, total: this.state.total + total }
  }

  setUnits(units: ProgressUnits | null): void {
    if (this.state === true) return
    if (this.state === null || this.state === false) {
      this.state = { done: 0, total: null, units }
      return
    }
    this.state = { ...this.state, units }
  }

  complete(): void {
    this.state = true
  }

  /** Replace this phase's value wholesale (e.g. a precomputed Progress). */
  setRaw(value: Progress): void {
    this.state = value
  }

  snapshot(): PhaseProgress {
    return this.state
  }
}

function isNotStarted(p: PhaseProgress): boolean {
  if (p === null) return true
  if (typeof p === 'boolean') return false
  if ('overall' in p) return isNotStartedProgress(p.overall)
  return false
}

function isNotStartedProgress(p: Progress): boolean {
  return p === null
}

function phaseRatio(p: PhaseProgress): number {
  if (p === null) return 0
  if (typeof p === 'boolean') return p ? 1 : 0
  if ('overall' in p) return progressRatio(p.overall)
  return progressRatio(p as Progress)
}

function progressRatio(p: Progress): number {
  if (p === true) return 1
  if (p === false || p === null) return 0
  if (p.total === null || p.total <= 0) return 0
  return Math.min(1, p.done / p.total)
}
