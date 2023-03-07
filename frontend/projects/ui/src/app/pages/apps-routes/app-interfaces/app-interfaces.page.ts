import {
  ChangeDetectionStrategy,
  Component,
  Input,
  Pipe,
  PipeTransform,
} from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ModalController, ToastController } from '@ionic/angular'
import { getPkgId, copyToClipboard } from '@start9labs/shared'
import { AddressInfo, DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { QRComponent } from 'src/app/components/qr/qr.component'
import { map } from 'rxjs'

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)
  readonly addressInfo$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'address-info')
    .pipe(
      map(addressInfo =>
        Object.values(addressInfo).sort((a, b) => a.name.localeCompare(b.name)),
      ),
    )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input()
  addressInfo!: AddressInfo

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly modalCtrl: ModalController,
  ) {}

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  async showQR(text: string): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: QRComponent,
      componentProps: {
        text,
      },
      cssClass: 'qr-modal',
    })
    await modal.present()
  }

  async copy(address: string): Promise<void> {
    let message = ''
    await copyToClipboard(address || '').then(success => {
      message = success
        ? 'Copied to clipboard!'
        : 'Failed to copy to clipboard.'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }
}

@Pipe({
  name: 'addressType',
})
export class AddressTypePipe implements PipeTransform {
  transform(address: string): string {
    if (isValidIpv4(address)) return 'IPv4'
    if (isValidIpv6(address)) return 'IPv6'

    const hostname = new URL(address).hostname
    if (hostname.endsWith('.onion')) return 'Tor'
    if (hostname.endsWith('.local')) return 'Local'

    return 'Custom'
  }
}

function isValidIpv4(address: string): boolean {
  const regexExp =
    /^(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$/
  return regexExp.test(address)
}

function isValidIpv6(address: string): boolean {
  const regexExp =
    /(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))/gi
  return regexExp.test(address)
}
