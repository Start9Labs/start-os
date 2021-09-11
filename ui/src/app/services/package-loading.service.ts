import { Injectable } from '@angular/core'
import { InstallProgress } from './patch-db/data-model'


@Injectable({
  providedIn: 'root',
})
export class PackageLoadingService {
  constructor () { }

  transform (loadData: InstallProgress): ProgressData {
    let { downloaded, validated, unpacked, size, 'download-complete': downloadComplete, 'validation-complete': validationComplete, 'unpack-complete': unpackComplete } = loadData
    // only permit 100% when "complete" == true
    downloaded = downloadComplete ? size : Math.max(downloaded - 1, 0)
    validated = validationComplete ? size : Math.max(validated - 1, 0)
    unpacked = unpackComplete ? size : Math.max(unpacked - 1, 0)

    const downloadWeight = 1
    const validateWeight = .2
    const unpackWeight = .7

    const numerator = Math.floor(
      downloadWeight * downloaded +
      validateWeight * validated +
      unpackWeight * unpacked)

    const denominator = Math.floor(size * (downloadWeight + validateWeight + unpackWeight))

    return {
      totalProgress: Math.floor(100 * numerator / denominator),
      downloadProgress: Math.floor(100 * downloaded / size),
      validateProgress: Math.floor(100 * validated / size),
      unpackProgress: Math.floor(100 * unpacked / size),
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