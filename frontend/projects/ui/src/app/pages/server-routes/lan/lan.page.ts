import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
})
export class LANPage {
  downloadIsDisabled: boolean

  readonly server$ = this.patch.watch$('server-info')

  constructor(
    private readonly config: ConfigService,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    this.downloadIsDisabled = !this.config.isTor()
  }

  installCert(): void {
    document.getElementById('install-cert').click()
  }
}
