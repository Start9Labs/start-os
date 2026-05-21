import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { phaseLeaf } from '../util/format-progress'

@Pipe({ name: 'phaseLeaf' })
export class PhaseLeafPipe implements PipeTransform {
  transform(progress: T.PhaseProgress): T.Progress {
    return phaseLeaf(progress)
  }
}
