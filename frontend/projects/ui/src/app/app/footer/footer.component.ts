import { ChangeDetectionStrategy, Component } from '@angular/core'
import { heightCollapse } from '../../util/animations'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs/operators'
import { DataModel } from '../../services/patch-db/data-model'

@Component({
  selector: 'footer[appFooter]',
  templateUrl: 'footer.component.html',
  styleUrls: ['footer.component.scss'],
  animations: [heightCollapse],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class FooterComponent {
  readonly progress$ = this.patch
    .watch$('server-info', 'status-info', 'update-progress')
    .pipe(map(a => a && { ...a }))

  readonly animation = {
    value: '',
    params: {
      duration: 1000,
      delay: 50,
    },
  }

  constructor(private readonly patch: PatchDB<DataModel>) {}

  getProgress(size: number, downloaded: number): number {
    return Math.round((100 * downloaded) / (size || 1))
  }
}
