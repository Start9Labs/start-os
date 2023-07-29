import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

export type ClearnetForm = {
  domain: string
  subdomain: string | null
}

@Component({
  selector: 'os-addresses',
  templateUrl: './os-addresses.page.html',
  styleUrls: ['./os-addresses.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class OSAddressesPage {
  readonly ui$ = this.patch.watch$('server-info', 'ui')

  constructor(private readonly patch: PatchDB<DataModel>) {}
}
