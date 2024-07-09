import { Pipe, PipeTransform } from '@angular/core'

const Mustache = require('mustache')

@Pipe({
  name: 'mustache',
})
export class MustachePipe implements PipeTransform {
  transform(value: any, displayAs: string): string {
    return displayAs && Mustache.render(displayAs, value)
  }
}
