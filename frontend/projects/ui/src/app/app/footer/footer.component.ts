import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

import { heightCollapse } from '../../util/animations'
import { PatchDbService } from '../../services/patch-db/patch-db.service'
import { map } from 'rxjs/operators'
import { ServerInfo } from '../../services/patch-db/data-model'

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

  constructor(private readonly patch: PatchDbService) {}

  getProgress({
    downloaded,
    size,
  }: ServerInfo['status-info']['update-progress']): number {
    return Math.round((100 * (downloaded || 1)) / (size || 1))
  }
}
