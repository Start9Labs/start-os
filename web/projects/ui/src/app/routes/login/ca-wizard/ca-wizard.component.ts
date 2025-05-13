import { CommonModule, DOCUMENT } from '@angular/common'
import { Component, inject } from '@angular/core'
import { DocsLinkDirective, i18nPipe, RELATIVE_URL } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiSurface } from '@taiga-ui/core'
import { TuiCardLarge } from '@taiga-ui/layout'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'ca-wizard',
  templateUrl: './ca-wizard.component.html',
  styleUrls: ['./ca-wizard.component.scss'],
  imports: [
    CommonModule,
    TuiIcon,
    TuiButton,
    TuiCardLarge,
    TuiSurface,
    i18nPipe,
    DocsLinkDirective,
  ],
})
export class CAWizardComponent {
  private readonly api = inject(ApiService)
  private readonly relativeUrl = inject(RELATIVE_URL)
  private readonly document = inject(DOCUMENT)

  readonly config = inject(ConfigService)
  caTrusted = false

  async ngOnInit() {
    await this.testHttps().catch(e =>
      console.warn('Failed Https connection attempt'),
    )
  }

  refresh() {
    this.document.location.reload()
  }

  launchHttps() {
    this.document.defaultView?.open(`https://${this.config.getHost()}`, '_self')
  }

  private async testHttps() {
    const url = `https://${this.document.location.host}${this.relativeUrl}`
    await this.api.echo({ message: 'ping' }, url).then(() => {
      this.caTrusted = true
    })
  }
}
