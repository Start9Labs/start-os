import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
})
export class LANPage {
  lanAddress: string
  lanDisabled: string

  constructor (
    private readonly config: ConfigService,
  ) { }

  ngOnInit () {
    if (!this.config.isTor()) {
      this.lanDisabled = 'For security reasons, you must setup LAN over a Tor connection. Please navigate to your Embassy Tor Address and try again.'
    }
  }

  installCert (): void {
    document.getElementById('install-cert').click()
  }
}
