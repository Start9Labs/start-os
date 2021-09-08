import { Injectable } from '@angular/core'
import { InstallProgress } from './patch-db/data-model'


@Injectable({
  providedIn: 'root',
})
export class PackageLoadingService {
  constructor () { }

  transform (loadData: InstallProgress): ProgressData {
    let { downloaded, validated, unpacked, size, 'download-complete': downloadComplete, 'validation-complete': validationComplete, 'unpack-complete': unpackComplete } = loadData
    downloaded = downloadComplete ? size : downloaded
    validated = validationComplete ? size : validated
    unpacked = unpackComplete ? size : unpacked

    const downloadWeight = 1
    const validateWeight = .2
    const unpackWeight = .7

    const numerator = Math.floor(
      downloadWeight * downloaded +
      validateWeight * validated +
      unpackWeight * unpacked)

    const denominator = Math.floor(size * (downloadWeight + validateWeight + unpackWeight))

    return {
      totalProgress: Math.round(100 * numerator / denominator),
      downloadProgress: Math.round(100 * downloaded / size),
      validateProgress: Math.round(100 * validated / size),
      unpackProgress: Math.round(100 * unpacked / size),
      isComplete: downloadComplete && validationComplete && unpackComplete,
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