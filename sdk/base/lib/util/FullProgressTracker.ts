import { FullProgress, Progress, ProgressUnits } from '../osBindings'

/**
 * Pushes a snapshot to the host. The harness bakes the effects context in at
 * construction (`(progress) => effects.setInitProgress({ progress })` for
 * init, `setBackupProgress` for backup) so service code never touches the
 * effect — or the effects context — directly.
 */
export type ProgressSink = (progress: Progress) => Promise<unknown>

/**
 * Mirror of the core Rust `FullProgressTracker`. Use this when a service's
 * init / backup / restore procedure has internal phases it wants to surface
 * to the host's progress UI.
 *
 * Service code does not call the progress effect directly, and usually does
 * not call `sync()` either. The init and backup harnesses build a root tracker
 * (with the effects context baked in) and hand each handler its own tracker.
 * **Every phase update auto-syncs in the background** — just add phases and
 * update them. On the wire a snapshot lands as `Progress::Nested(FullProgress)`.
 *
 * Auto-sync is coalesced: at most one report is in flight and one queued. A
 * burst of updates collapses to the latest snapshot, so promises never stack
 * up. `sync()` is the explicit flush — it resolves once the in-flight and
 * queued reports have drained — handy before a handler returns.
 *
 * Phases can be nested: `addNestedPhase` returns a child tracker whose
 * snapshot the parent folds in as a `Progress::Nested(...)` value, and whose
 * updates bubble up to this tracker's auto-sync.
 *
 * A handler that doesn't hold onto the handle from `addPhase` can fetch it
 * back later by name with `getPhase(name)` (or `getNestedPhase(name)`).
 */
export class FullProgressTracker {
  private phases: Array<{
    name: string
    contribution: number | null
    handle?: PhaseHandle
    child?: FullProgressTracker
  }> = []
  private completed = false
  private parent?: FullProgressTracker
  /** Root-only: the push currently draining, or null when idle. */
  private inFlight: Promise<void> | null = null
  /** Root-only: a newer update arrived while a push was in flight. */
  private queued = false

  constructor(private readonly pushEffect?: ProgressSink) {}

  private root(): FullProgressTracker {
    return this.parent ? this.parent.root() : this
  }

  /** Called by phases (directly or via nested children) on every update. */
  private notifyChange(): void {
    this.root().scheduleSync()
  }

  /** Root-only. Start a drain, or mark a follow-up if one is already running. */
  private scheduleSync(): void {
    if (!this.pushEffect) return
    if (this.inFlight) {
      this.queued = true
      return
    }
    this.inFlight = this.drain()
  }

  private async drain(): Promise<void> {
    // Each iteration snapshots fresh, so a coalesced burst reports its latest
    // state. Loop while updates kept arriving during the previous push.
    while (true) {
      const snapshot = this.snapshot()
      try {
        await this.pushEffect!(snapshot)
      } catch {
        // best-effort; a no-op outside the relevant transition
      }
      if (!this.queued) break
      this.queued = false
    }
    this.inFlight = null
  }

  addPhase(name: string, contribution: number | null = 1): PhaseHandle {
    const handle = new PhaseHandle(() => this.notifyChange())
    this.phases.push({ name, contribution, handle })
    return handle
  }

  addNestedPhase(
    name: string,
    contribution: number | null = 1,
  ): FullProgressTracker {
    const child = new FullProgressTracker()
    child.parent = this
    this.phases.push({ name, contribution, child })
    return child
  }

  /**
   * Retrieve the handle for a leaf phase added earlier by `name`. Returns
   * `undefined` if no such phase exists (or it was added as a nested phase).
   */
  getPhase(name: string): PhaseHandle | undefined {
    return this.phases.find(p => p.name === name && p.handle)?.handle
  }

  /**
   * Retrieve the child tracker for a nested phase added earlier by `name`.
   * Returns `undefined` if no such phase exists (or it was added as a leaf).
   */
  getNestedPhase(name: string): FullProgressTracker | undefined {
    return this.phases.find(p => p.name === name && p.child)?.child
  }

  /** Mark the overall progress as complete. Does not mutate individual phases. */
  complete(): void {
    this.completed = true
    this.notifyChange()
  }

  /**
   * Flush: push the current state and resolve once the in-flight and queued
   * reports have drained. Auto-sync already fires on every update, so this is
   * only needed to guarantee the final state has landed before returning.
   * No-op for a root with no sink (e.g. a detached tracker).
   */
  async sync(): Promise<void> {
    const root = this.root()
    root.scheduleSync()
    while (root.inFlight) await root.inFlight
  }

  private phaseValue(entry: {
    handle?: PhaseHandle
    child?: FullProgressTracker
  }): Progress {
    if (entry.child) return entry.child.snapshot()
    return entry.handle ? entry.handle.snapshot() : null
  }

  snapshot(): FullProgress {
    return {
      overall: this.completed ? true : this.computeOverall(),
      phases: this.phases.map(p => ({
        name: p.name,
        progress: this.phaseValue(p),
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
      const v = this.phaseValue(p)
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

  /**
   * @param onChange - fired after every mutation so the owning tracker can
   * auto-sync. Defaults to a no-op for standalone handles.
   */
  constructor(private readonly onChange: () => void = () => {}) {}

  start(): void {
    if (this.state === null) this.state = false
    this.onChange()
  }

  setDone(done: number): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done, total: null, units: null }
      this.onChange()
      return
    }
    const clamped =
      this.state.total !== null ? Math.min(done, this.state.total) : done
    this.state = { ...this.state, done: clamped }
    this.onChange()
  }

  setTotal(total: number): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done: 0, total, units: null }
      this.onChange()
      return
    }
    this.state = { ...this.state, total }
    this.onChange()
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
    this.onChange()
  }

  setUnits(units: ProgressUnits | null): void {
    if (this.state === true) return
    if (
      this.state === null ||
      this.state === false ||
      isFullProgress(this.state)
    ) {
      this.state = { done: 0, total: null, units }
      this.onChange()
      return
    }
    this.state = { ...this.state, units }
    this.onChange()
  }

  complete(): void {
    this.state = true
    this.onChange()
  }

  /** Replace this phase's value wholesale — accepts any `Progress`, including a nested `FullProgress`. */
  setRaw(value: Progress): void {
    this.state = value
    this.onChange()
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
