import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'

export interface InstallPhaseView {
  // Progress driving the row's status + bar. When the phase nests a service's
  // setInitProgress, this is the furthest-along sub-phase's progress (so the bar
  // "reloads" for each sub-phase); otherwise the phase's own progress.
  progress: T.Progress
  // The furthest-along sub-phase's name, shown as subtext — null when the phase
  // has no nested sub-phases or none has started yet.
  subtext: string | null
}

@Pipe({ name: 'installPhaseView' })
export class InstallPhaseViewPipe implements PipeTransform {
  transform(progress: T.Progress): InstallPhaseView {
    if (
      progress !== null &&
      typeof progress === 'object' &&
      'overall' in progress
    ) {
      // The last sub-phase that has started: the running one while installing,
      // and the final one once they're all done — so the subtext stays put
      // through completion instead of dropping off and shrinking the card.
      const started = progress.phases.filter(p => p.progress !== null)
      const current = started[started.length - 1]

      return current
        ? { progress: current.progress, subtext: current.name }
        : { progress, subtext: null }
    }

    return { progress, subtext: null }
  }
}
