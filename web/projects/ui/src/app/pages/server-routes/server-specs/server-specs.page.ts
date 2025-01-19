import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ToastController } from '@ionic/angular'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { copyToClipboard } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { map, Observable } from 'rxjs'
import {
  getAddresses,
  MappedInterface,
} from 'src/app/components/interface-info/interface-info.component'

const iface = {
  id: '',
  name: 'StartOS User Interface',
  description:
    'The primary user interface for your StartOS server, accessible from any browser.',
  type: 'ui' as const,
  masked: false,
  addressInfo: {
    hostId: '',
    internalPort: 80,
    scheme: 'http',
    sslScheme: 'https',
    suffix: '',
    username: null,
  },
}

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ServerSpecsPage {
  readonly server$ = this.patch.watch$('serverInfo')

  readonly ui$ = this.server$.pipe(
    map(server => ({
      ...iface,
      public: server.host.bindings[iface.addressInfo.internalPort].net.public,
      addresses: getAddresses(iface, server.host),
    })),
  )

  constructor(
    private readonly toastCtrl: ToastController,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
  ) {}

  get gitHash(): string {
    return this.config.gitHash
  }

  async copy(address: string) {
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
