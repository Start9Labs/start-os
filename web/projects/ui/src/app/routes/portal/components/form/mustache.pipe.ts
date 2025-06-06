import { Pipe, PipeTransform } from '@angular/core'
import Mustache from 'mustache'

@Pipe({
  name: 'mustache',
  standalone: false,
})
export class MustachePipe implements PipeTransform {
  transform(value: any, displayAs: string): string {
    return displayAs && Mustache.render(displayAs, value)
  }
}
