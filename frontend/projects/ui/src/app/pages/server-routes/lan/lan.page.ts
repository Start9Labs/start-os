import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDB } from 'patch-db-client'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LANPage {
  readonly downloadIsDisabled = !this.config.isTor()
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly config: ConfigService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  installCert(): void {
    document.getElementById('install-cert')?.click()
  }
}
