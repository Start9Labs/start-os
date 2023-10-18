import { Component, Inject } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { pauseFor, RELATIVE_URL } from '@start9labs/shared'
import { DOCUMENT } from '@angular/common'
import { WINDOW } from '@ng-web-apis/common'

@Component({
  selector: 'ca-wizard',
  templateUrl: './ca-wizard.component.html',
  styleUrls: ['./ca-wizard.component.scss'],
})
export class CAWizardComponent {
  downloadClicked = false
  instructionsClicked = false
  polling = false
  caTrusted = false

  constructor(
    private readonly api: ApiService,
    public readonly config: ConfigService,
    @Inject(RELATIVE_URL) private readonly relativeUrl: string,
    @Inject(DOCUMENT) public readonly document: Document,
    @Inject(WINDOW) private readonly windowRef: Window,
  ) {}

  async ngOnInit() {
    if (!this.config.isSecure()) {
      await this.testHttps().catch(e =>
        console.warn('Failed Https connection attempt'),
      )
    }
  }

  download() {
    this.downloadClicked = true
    this.document.getElementById('install-cert')?.click()
  }

  instructions() {
    this.windowRef.open(
      'https://docs.start9.com/0.3.5.x/getting-started/trust-ca/#trust-root-ca',
      '_blank',
      'noreferrer',
    )
    this.instructionsClicked = true
    this.startDaemon()
  }

  private async startDaemon(): Promise<void> {
    this.polling = true
    while (this.polling) {
      try {
        await this.testHttps()
        this.polling = false
      } catch (e) {
        console.warn('Failed Https connection attempt')
        await pauseFor(2000)
      }
    }
  }

  launchHttps() {
    const host = this.config.getHost()
    this.windowRef.open(`https://${host}`, '_blank', 'noreferrer')
  }

  private async testHttps() {
    const url = `https://${this.document.location.host}${this.relativeUrl}`
    await this.api.echo({ message: 'ping' }, url).then(() => {
      this.downloadClicked = true
      this.instructionsClicked = true
      this.caTrusted = true
    })
  }
}
