import { Pipe, PipeTransform } from '@angular/core'
import { InstallProgress } from '../services/patch-db/data-model'

@Pipe({
  name: 'installState',
})
export class InstallState implements PipeTransform {

  transform (loadData: InstallProgress): ProgressData {
    const downloadWeight = 1
    const validateWeight = .1
    const unpackWeight = .2

    const numerator = Math.floor(
      downloadWeight * loadData.downloaded +
      validateWeight * loadData.validated +
      unpackWeight * loadData.unpacked)

    const denominator = Math.floor(loadData.size * (downloadWeight + validateWeight + unpackWeight))

    return {
      totalProgress: Math.round(100 * numerator / denominator),
      downloadProgress: Math.round(100 * loadData.downloaded / loadData.size),
      validateProgress: Math.round(100 * loadData.validated / loadData.size),
      unpackProgress: Math.round(100 * loadData.unpacked / loadData.size),
      isComplete: loadData['download-complete'] && loadData['validation-complete'] && loadData['unpack-complete'],
    }
  }
}

export interface ProgressData {
  totalProgress: number
  downloadProgress: number
  validateProgress: number
  unpackProgress: number
  isComplete: boolean
}