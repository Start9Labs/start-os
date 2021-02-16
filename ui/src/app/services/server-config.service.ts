import { Injectable } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { AppConfigValuePage } from '../modals/app-config-value/app-config-value.page'
import { ApiService } from './api/api.service'
import { ConfigSpec } from '../pkg-config/config-types'
import { ConfigCursor } from '../pkg-config/config-cursor'
import { SSHService } from '../pages/server-routes/developer-routes/dev-ssh-keys/ssh.service'

@Injectable({
  providedIn: 'root',
})
export class ServerConfigService {

  constructor (
    private readonly modalCtrl: ModalController,
    private readonly apiService: ApiService,
    private readonly sshService: SSHService,
  ) { }

  async presentModalValueEdit (key: string, current?: string) {
    const cursor = new ConfigCursor(serverConfig, { [key]: current }).seekNext(key)

    const modal = await this.modalCtrl.create({
      backdropDismiss: false,
      component: AppConfigValuePage,
      presentingElement: await this.modalCtrl.getTop(),
      componentProps: {
        cursor,
        saveFn: this.saveFns[key],
      },
    })

    await modal.present()
  }

  saveFns: { [key: string]: (val: any) => Promise<any> } = {
    name: async (value: string) => {
      return this.apiService.setDbValue({ pointer: 'ui/name', value })
    },
    autoCheckUpdates: async (value: boolean) => {
      return this.apiService.setDbValue({ pointer: 'ui/auto-check-updates', value })
    },
    ssh: async (pubkey: string) => {
      return this.sshService.add(pubkey)
    },
    registry: async (url: string) => {
      return this.apiService.setRegistry({ url })
    },
    // password: async (password: string) => {
    //   return this.apiService.updatePassword({ password })
    // },
  }
}

const serverConfig: ConfigSpec = {
  name: {
    type: 'string',
    name: 'Device Name',
    description: 'A unique label for this device.',
    nullable: false,
    // @TODO determine regex
    // pattern: '',
    patternDescription: 'Must be less than 40 characters',
    masked: false,
    copyable: false,
  },
  autoCheckUpdates: {
    type: 'boolean',
    name: 'Auto Check for Updates',
    description: 'On launch, EmabssyOS will automatically check for updates of itself and your installed services. Updating still requires user approval and action. No updates will ever be performed automatically.',
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
  registry: {
    type: 'string',
    name: 'Marketplace URL',
    description: 'The URL of the service marketplace. By default, your Embassy connects to the official Start9 Embassy Marketplace.',
    nullable: true,
    // @TODO regex for URL
    // pattern: '',
    patternDescription: 'Must be a valid URL',
    changeWarning: 'Downloading services from an alternative marketplace can result in malicious or harmful code being installed on your device.',
    default: 'https://registry.start9.com',
    masked: false,
    copyable: false,
  },
  // password: {
  //   type: 'string',
  //   name: 'Change Password',
  //   description: 'The master password for your Embassy. Must contain at least 128 bits of entropy.',
  //   nullable: false,
  //   // @TODO figure out how to confirm min entropy
  //   // pattern: '^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[*.!@#$%^&*\]).{12,32}$',
  //   patternDescription: 'Password too simple. Password must contain at least 128 bits of entroy.',
  //   changeWarning: 'Changing your password will have no affect on old backups. In order to restore old backups, you must provide the password that was used to create them.',
  //   masked: true,
  //   copyable: true,
  // },
}
