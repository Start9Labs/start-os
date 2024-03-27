import { Pipe, PipeTransform } from '@angular/core'

@Pipe({
  name: 'duration',
  standalone: true,
})
export class DurationPipe implements PipeTransform {
  transform(start: string, finish: string): number {
    return (new Date(finish).valueOf() - new Date(start).valueOf()) / 100
  }
}
