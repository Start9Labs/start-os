import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'durationToSeconds',
})
export class DurationToSecondsPipe implements PipeTransform {
  transform (duration: string | null): number {
    if (!duration) return 0
    const splitUnit = duration.match(/^([0-9]*(\.[0-9]+)?)(ns|µs|ms|s|m|d)$/)
    const unit = splitUnit[3]
    const num = splitUnit[1]
    return Number(num) * unitsToSeconds[unit]
  }
}

const unitsToSeconds = {
  'ns': 1e-9,
  'µs': 1e-6,
  'ms': 0.001,
  's': 1,
  'm': 60,
  'h': 3600,
  'd': 86400,
}