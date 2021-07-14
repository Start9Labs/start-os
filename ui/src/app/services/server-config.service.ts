import { Injectable } from '@angular/core'
import { AppConfigValuePage } from '../modals/app-config-value/app-config-value.page'
import { ApiService } from './api/api.service'
import { ConfigSpec } from '../pkg-config/config-types'
import { ConfigCursor } from '../pkg-config/config-cursor'
import { SSHService } from '../pages/server-routes/developer-routes/dev-ssh-keys/ssh.service'
import { TrackingModalController } from './tracking-modal-controller.service'

@Injectable({
  providedIn: 'root',
})
export class ServerConfigService {

  constructor (
    private readonly trackingModalCtrl: TrackingModalController,
    private readonly apiService: ApiService,
    private readonly sshService: SSHService,
  ) { }

  async presentModalValueEdit (key: string, current?: string) {
    const cursor = new ConfigCursor(serverConfig, { [key]: current }).seekNext(key)

    const modal = await this.trackingModalCtrl.create({
      backdropDismiss: false,
      component: AppConfigValuePage,
      presentingElement: await this.trackingModalCtrl.getTop(),
      componentProps: {
        cursor,
        saveFn: this.saveFns[key],
      },
    })

    await modal.present()
  }

  saveFns: { [key: string]: (val: any) => Promise<any> } = {
    autoCheckUpdates: async (value: boolean) => {
      return this.apiService.setDbValue({ pointer: 'ui/auto-check-updates', value })
    },
    ssh: async (pubkey: string) => {
      return this.sshService.add(pubkey)
    },
    eosMarketplace: async (enabled: boolean) => {
      return this.apiService.setEosMarketplace(enabled)
    },
    packageMarketplace: async (url: string) => {
      return this.apiService.setPackageMarketplace({ url })
    },
    password: async (password: string) => {
      return this.apiService.updatePassword({ password })
    },
  }
}

const serverConfig: ConfigSpec = {
  autoCheckUpdates: {
    type: 'boolean',
    name: 'Auto Check for Updates',
    description: 'On launch, EmbassyOS will automatically check for updates of itself and your installed services. Updating still requires user approval and action. No updates will ever be performed automatically.',
    default: true,
  },
  ssh: {
    type: 'string',
    name: 'SSH Key',
    description: 'Add SSH keys to your Embassy to gain root access from the command line.',
    nullable: false,
    // @TODO regex for SSH Key
    // pattern: '',
    patternDescription: 'Must be a valid SSH key',
    masked: false,
    copyable: false,
  },
  eosMarketplace: {
    type: 'boolean',
    name: 'Use Tor',
    description: `Use Start9's Tor Hidden Service Marketplace (instead of clearnet).`,
    default: false,
  },
  packageMarketplace: {
    type: 'string',
    name: 'Package Marketplace',
    description: `Use for alternative embassy marketplace. Leave empty to use start9's marketplace.`,
    nullable: true,
    // @TODO regex for URL
    // pattern: '',
    patternDescription: 'Must be a valid URL.',
    masked: false,
    copyable: false,
  },
  password: {
    type: 'string',
    name: 'Change Password',
    description: `Your Embassy's master password, used for authentication and disk encryption.`,
    nullable: false,
    // @TODO regex for 12 chars
    // pattern: '',
    patternDescription: 'Must contain at least 12 characters.',
    changeWarning: 'If you forget your master password, there is absolutely no way to recover your data. This can result in loss of money! Keep in mind, old backups will still be encrypted by the password used to encrypt them.',
    masked: false,
    copyable: false,
  },
}
