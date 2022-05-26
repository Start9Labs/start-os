import { Component, Input, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent, ToastController } from '@ionic/angular'
import { getPkgId } from '@start9labs/shared'
import { getUiInterfaceKey } from 'src/app/services/config.service'
import {
  InstalledPackageDataEntry,
  InterfaceDef,
} from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { copyToClipboard } from 'src/app/util/web.util'

interface LocalInterface {
  def: InterfaceDef
  addresses: InstalledPackageDataEntry['interface-addresses'][string]
}

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  @ViewChild(IonContent) content: IonContent
  ui: LocalInterface | null
  other: LocalInterface[] = []
  readonly pkgId = getPkgId(this.route)

  constructor(
    private readonly route: ActivatedRoute,
    public readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    const pkg = this.patch.getData()['package-data'][this.pkgId]
    const interfaces = pkg.manifest.interfaces
    const uiKey = getUiInterfaceKey(interfaces)

    if (!pkg?.installed) return

    const addressesMap = pkg.installed['interface-addresses']

    if (uiKey) {
      const uiAddresses = addressesMap[uiKey]
      this.ui = {
        def: interfaces[uiKey],
        addresses: {
          'lan-address': uiAddresses['lan-address']
            ? 'https://' + uiAddresses['lan-address']
            : '',
          'tor-address': uiAddresses['tor-address']
            ? 'http://' + uiAddresses['tor-address']
            : '',
        },
      }
    }

    this.other = Object.keys(interfaces)
      .filter(key => key !== uiKey)
      .map(key => {
        const addresses = addressesMap[key]
        return {
          def: interfaces[key],
          addresses: {
            'lan-address': addresses['lan-address']
              ? 'https://' + addresses['lan-address']
              : '',
            'tor-address': addresses['tor-address']
              ? 'http://' + addresses['tor-address']
              : '',
          },
        }
      })
  }

  ngAfterViewInit() {
    this.content.scrollToPoint(undefined, 1)
  }

  asIsOrder() {
    return 0
  }
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input() interface: LocalInterface

  constructor(private readonly toastCtrl: ToastController) {}

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  async copy(address: string): Promise<void> {
    let message = ''
    await copyToClipboard(address || '').then(success => {
      message = success ? 'copied to clipboard!' : 'failed to copy'
    })

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }
}
