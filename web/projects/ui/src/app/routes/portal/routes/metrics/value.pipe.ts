import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  standalone: true,
  name: 'value',
})
export class ValuePipe implements PipeTransform {
  readonly transform = getValue
}

export function getValue(value?: string | null): number | string {
  return value == null ? '-' : Number.parseFloat(value)
}
