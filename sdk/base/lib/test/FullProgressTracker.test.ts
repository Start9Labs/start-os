import { FullProgressTracker, leafProgress } from '../util/FullProgressTracker'
import { FullProgress, Progress } from '../osBindings'

const flush = () => new Promise(resolve => setTimeout(resolve, 0))

function leafDone(p: Progress): number | null {
  const leaf = leafProgress(p)
  if (leaf === null || leaf === false || leaf === true) return null
  return leaf.done
}

/** `done` of the idx-th phase of a pushed root snapshot. */
function phaseDone(snap: Progress, idx = 0): number | null {
  const p = (snap as FullProgress).phases[idx]?.progress
  return p === undefined ? null : leafDone(p)
}

describe('FullProgressTracker auto-sync', () => {
  it('pushes in the background on update without an explicit sync', async () => {
    const pushed: Progress[] = []
    const tracker = new FullProgressTracker(p => {
      pushed.push(p)
      return Promise.resolve()
    })
    const phase = tracker.addPhase('p', 1)
    phase.setTotal(100)
    phase.setDone(42)
    await tracker.sync()
    expect(phaseDone(pushed[pushed.length - 1]!)).toBe(42)
  })

  it('coalesces a burst into one in-flight + one queued (latest wins)', async () => {
    const pushed: Progress[] = []
    const gates: Array<() => void> = []
    const tracker = new FullProgressTracker(p => {
      pushed.push(p)
      return new Promise<void>(resolve => gates.push(resolve))
    })
    const phase = tracker.addPhase('p', 1)

    phase.setTotal(100) // launches push #0 (in flight, done=0)
    for (let i = 1; i <= 50; i++) phase.setDone(i) // all coalesce into one queued

    // only the first push is in flight; the 50 updates are a single queued slot
    expect(pushed.length).toBe(1)
    expect(phaseDone(pushed[0]!)).toBe(0)

    gates[0]!() // in-flight resolves -> queued runs with the LATEST snapshot
    await flush()
    expect(pushed.length).toBe(2)
    expect(phaseDone(pushed[1]!)).toBe(50)

    gates[1]!() // nothing queued -> drain stops
    await flush()
    expect(pushed.length).toBe(2)
  })

  it('sync() resolves only once in-flight and queued have drained', async () => {
    const gates: Array<() => void> = []
    const tracker = new FullProgressTracker(
      () => new Promise<void>(resolve => gates.push(resolve)),
    )
    const phase = tracker.addPhase('p', 1)
    phase.setTotal(100) // push #0 in flight
    phase.setDone(10) // queued

    let settled = false
    const done = tracker.sync().then(() => {
      settled = true
    })

    await flush()
    expect(settled).toBe(false) // push #0 still in flight

    gates[0]!() // -> queued push #1 launches
    await flush()
    expect(settled).toBe(false) // queued push still in flight

    gates[1]!()
    await done
    expect(settled).toBe(true)
  })

  it('nested child updates bubble up to the root sink', async () => {
    const pushed: Progress[] = []
    const tracker = new FullProgressTracker(p => {
      pushed.push(p)
      return Promise.resolve()
    })
    const child = tracker.addNestedPhase('child', 1)
    const phase = child.addPhase('p', 1)
    phase.setTotal(10)
    phase.setDone(7)
    await tracker.sync()

    expect(pushed.length).toBeGreaterThan(0)
    const last = pushed[pushed.length - 1] as FullProgress
    const childProgress = last.phases[0]!.progress as FullProgress
    expect(leafDone(childProgress.phases[0]!.progress)).toBe(7)
  })

  it('a sink-less (detached) tracker no-ops without throwing', async () => {
    const tracker = new FullProgressTracker()
    const phase = tracker.addPhase('p', 1)
    phase.setTotal(5)
    phase.setDone(5)
    await expect(tracker.sync()).resolves.toBeUndefined()
  })
})
