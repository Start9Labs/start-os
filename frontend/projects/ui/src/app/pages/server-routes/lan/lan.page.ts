import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'lan',
  templateUrl: './lan.page.html',
  styleUrls: ['./lan.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class LANPage {
  installCert(): void {
    document.getElementById('install-cert')?.click()
  }
}
