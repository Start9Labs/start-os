import { Pipe, PipeTransform } from '@angular/core'

// converts bytes to gigabytes
@Pipe({
  name: 'convertBytes',
})
export class ConvertBytesPipe implements PipeTransform {
  transform(bytes: number): string {
    if (bytes === 0) return '0 Bytes'

    const k = 1024
    const i = Math.floor(Math.log(bytes) / Math.log(k))

    return parseFloat((bytes / Math.pow(k, i)).toFixed(1)) + ' ' + sizes[i]
  }
}

@Pipe({
  name: 'durationToSeconds',
})
export class DurationToSecondsPipe implements PipeTransform {
  transform(duration: string | null): number {
    if (!duration) return 0
    const splitUnit = duration.match(/^([0-9]*(\.[0-9]+)?)(ns|µs|ms|s|m|d)$/)
    const unit = splitUnit[3]
    const num = splitUnit[1]
    return Number(num) * unitsToSeconds[unit]
  }
}

const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB', 'ZB', 'YB']

const unitsToSeconds = {
  ns: 1e-9,
  µs: 1e-6,
  ms: 0.001,
  s: 1,
  m: 60,
  h: 3600,
  d: 86400,
}
