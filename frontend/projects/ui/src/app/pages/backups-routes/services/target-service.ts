import { Injectable } from '@angular/core'
import { LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { RR } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'

@Injectable({
  providedIn: 'root',
})
export class TargetService {
  constructor(
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
  ) {}

  async presentModalAddRemoteTarget(): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'New Network Folder',
        spec: RemoteBackupTargetSpec,
        buttons: [
          {
            text: 'Connect',
            handler: (value: RR.AddBackupTargetReq) => {
              return this.saveTarget(value)
            },
            isSubmit: true,
          },
        ],
      },
    })
    await modal.present()
  }

  async saveTarget(
    value: RR.AddBackupTargetReq,
  ): Promise<RR.AddBackupTargetRes | undefined> {
    const loader = await this.loadingCtrl.create({
      message: 'Saving target...',
    })
    await loader.present()

    try {
      return this.api.addBackupTarget(value)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}

export const DropboxSpec: ConfigSpec = {
  token: {
    type: 'string',
    name: 'Access Token',
    description: 'The secret access token for your custom Dropbox app',
    placeholder: `paste access token`,
    nullable: false,
    masked: true,
    copyable: false,
  },
}

export const GoogleDriveSpec: ConfigSpec = {
  key: {
    type: 'file',
    name: 'Private Key File',
    description:
      'Your Google Drive service account private key file (.json file)',
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    validation: () => true,
    nullable: false,
  },
}

export const CifsSpec: ConfigSpec = {
  hostname: {
    type: 'string',
    name: 'Hostname',
    description:
      'The hostname of your target device on the Local Area Network.',
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    pattern: '^[a-zA-Z0-9._-]+( [a-zA-Z0-9]+)*$',
    'pattern-description': `Must be a valid hostname. e.g. 'My Computer' OR 'my-computer.local'`,
    nullable: false,
    masked: false,
    copyable: false,
  },
  path: {
    type: 'string',
    name: 'Path',
    description: `On Windows, this is the fully qualified path to the shared folder, (e.g. /Desktop/my-folder).\n\n On Linux and Mac, this is the literal name of the shared folder (e.g. my-shared-folder).`,
    placeholder: 'e.g. my-shared-folder or /Desktop/my-folder',
    nullable: false,
    masked: false,
    copyable: false,
  },
  username: {
    type: 'string',
    name: 'Username',
    description: `On Linux, this is the samba username you created when sharing the folder.\n\n On Mac and Windows, this is the username of the user who is sharing the folder.`,
    nullable: false,
    masked: false,
    copyable: false,
  },
  password: {
    type: 'string',
    name: 'Password',
    description: `On Linux, this is the samba password you created when sharing the folder.\n\n On Mac and Windows, this is the password of the user who is sharing the folder.`,
    nullable: true,
    masked: true,
    copyable: false,
  },
}

export const RemoteBackupTargetSpec: ConfigSpec = {
  type: {
    type: 'union',
    variants: {
      cifs: CifsSpec,
      dropbox: DropboxSpec,
      'google-drive': GoogleDriveSpec,
    },
    tag: {
      id: 'type',
      name: 'Target Type',
      'variant-names': {
        cifs: 'Network Folder',
        dropbox: 'Dropbox',
        'google-drive': 'Google Drive',
      },
    },
    default: 'dropbox',
  },
}
