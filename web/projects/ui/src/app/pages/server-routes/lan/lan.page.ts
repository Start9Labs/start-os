import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LANPage {
  readonly crtName$ = this.patch
    .watch$('server-info', 'lan-address')
    .pipe(map(addr => `${new URL(addr).hostname}.crt`))

  constructor(private readonly patch: PatchDB<DataModel>) {}

  installCert(): void {
    document.getElementById('install-cert')?.click()
  }
}
