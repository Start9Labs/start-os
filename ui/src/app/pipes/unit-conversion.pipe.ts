import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'durationToSeconds',
})
export class DurationToSecondsPipe implements PipeTransform {
  transform (duration: string): number {
    if (!duration) return 0
    const unit = units.filter(u => duration.includes(u)).reduce((a, b) => a.length > b.length ? a : b )
    const num = duration.replace(unit, '')
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

const units = Object.keys(unitsToSeconds)
