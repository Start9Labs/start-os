import { Component, Input, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent, ToastController } from '@ionic/angular'
import { InterfaceDef, InterfaceInfo, PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ConfigService } from 'src/app/services/config.service'
import { copyToClipboard } from 'src/app/util/web.util'

interface LocalInterface {
  def: InterfaceDef
  addresses: InterfaceInfo['addresses'][string]
}

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesPage {
  @ViewChild(IonContent) content: IonContent
  ui: LocalInterface | null
  other: LocalInterface[]

  constructor (
    private readonly route: ActivatedRoute,
    private readonly toastCtrl: ToastController,
    private readonly config: ConfigService,
    public readonly patch: PatchDbService,
  ) { }

  ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')
    const pkg = this.patch.data['package-data'][pkgId]
    const interfaces = pkg.manifest.interfaces
    const addresses = pkg.installed['interface-info'].addresses
    const ui = interfaces['ui']

    if (ui) {
      this.ui = {
        def: ui,
        addresses: addresses['ui'],
      }
    }

    this.other = Object.keys(interfaces)
      .filter(key => key !== 'ui')
      .map(key => {
        return {
          def: interfaces[key],
          addresses: addresses[key],
        }
      })
  }

  ngAfterViewInit () {
    this.content.scrollToPoint(undefined, 1)
  }

  async copy (address: string): Promise<void> {
    let message = ''
    await copyToClipboard(address || '')
      .then(success => { message = success ? 'copied to clipboard!' : 'failed to copy'})

    const toast = await this.toastCtrl.create({
      header: message,
      position: 'bottom',
      duration: 1000,
    })
    await toast.present()
  }

  launch (pkg: PackageDataEntry): void {
    window.open(this.config.launchableURL(pkg), '_blank')
  }

  asIsOrder () {
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
}
