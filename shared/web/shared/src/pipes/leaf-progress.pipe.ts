import { Pipe, PipeTransform } from '@angular/core'
import { T } from '@start9labs/start-sdk'
import { leafProgress, LeafProgress } from '../util/format-progress'

@Pipe({ name: 'leafProgress' })
export class LeafProgressPipe implements PipeTransform {
  transform(progress: T.Progress): LeafProgress {
    return leafProgress(progress)
  }
}
