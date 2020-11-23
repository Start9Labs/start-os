import { Pipe, PipeTransform } from '@angular/core'
import { AppStatus } from '../models/app-model'
import { AppStatusRendering } from '../util/status-rendering'

@Pipe({
  name: 'displayBulb',
})
export class DisplayBulbPipe implements PipeTransform {

  transform (status: AppStatus, d: DisplayBulb): boolean {
    switch (AppStatusRendering[status].color) {
      case 'danger': return d === 'red'
      case 'success': return d === 'green'
      case 'warning': return d === 'yellow'
      default: return d === 'off'
    }
  }

}

type DisplayBulb = 'off' | 'red' | 'green' | 'yellow'
