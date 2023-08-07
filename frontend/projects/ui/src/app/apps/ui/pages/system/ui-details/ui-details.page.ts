import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'ui-details',
  templateUrl: './ui-details.page.html',
  styleUrls: ['./ui-details.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class UIDetailsPage {
  readonly ui$ = this.patch.watch$('server-info', 'ui')

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
