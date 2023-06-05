import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'port-forwards',
  templateUrl: './port-forwards.page.html',
  styleUrls: ['./port-forwards.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PortForwardsPage {
  readonly server$ = this.patch.watch$('server-info')

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
