import { inject, Pipe, PipeTransform } from '@angular/core'
import { BadgeService } from '../services/badge.service'
import { Observable } from 'rxjs'

@Pipe({
  name: 'toBadge',
  standalone: true,
})
export class ToBadgePipe implements PipeTransform {
  readonly badge = inject(BadgeService)

  transform(id: string): Observable<number> {
    return this.badge.getCount(id)
  }
}
