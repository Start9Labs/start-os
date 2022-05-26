import { Component } from '@angular/core'
import { ConfigService } from 'src/app/services/config.service'

const LAN_DISABLED =
  'For security reasons, you must setup LAN over a Tor connection. Please navigate to your Embassy Tor Address and try again.'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
})
export class LANPage {
  lanAddress = ''
  lanDisabled = this.config.isTor() ? '' : LAN_DISABLED

  constructor(private readonly config: ConfigService) {}

  installCert(): void {
    document.getElementById('install-cert')?.click()
  }
}
