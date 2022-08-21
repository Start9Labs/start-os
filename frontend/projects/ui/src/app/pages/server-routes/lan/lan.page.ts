import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

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
    private readonly patch: PatchDbService,
  ) {}

  installCert(): void {
    document.getElementById('install-cert')?.click()
  }
}
