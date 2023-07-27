import { inject, Pipe, PipeTransform } from '@angular/core'
import { Action } from '../components/actions/actions.component'
import { filter, map, Observable } from 'rxjs'
import { DesktopService } from '../routes/desktop/desktop.service'

@Pipe({
  name: 'toDesktopActions',
  standalone: true,
})
export class ToDesktopActionsPipe implements PipeTransform {
  private readonly desktop = inject(DesktopService)

  transform(
    value: Record<string, readonly Action[]>,
    id: string,
  ): Observable<Record<string, readonly Action[]>> {
    return this.desktop.desktop$.pipe(
      filter(Boolean),
      map(desktop => {
        const action = desktop.includes(id)
          ? {
              icon: 'tuiIconMinus',
              label: 'Remove from Desktop',
              action: () => this.desktop.remove(id),
            }
          : {
              icon: 'tuiIconPlus',
              label: 'Add to Desktop',
              action: () => this.desktop.add(id),
            }

        return {
          manage: [action],
        }
      }),
    )
  }
}
