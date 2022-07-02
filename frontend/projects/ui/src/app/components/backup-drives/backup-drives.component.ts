import { Component, EventEmitter, Input, Output } from '@angular/core'
import { BackupService } from './backup.service'
import {
  CifsBackupTarget,
  DiskBackupTarget,
  RR,
} from 'src/app/services/api/api.types'
import {
  ActionSheetController,
  AlertController,
  LoadingController,
  ModalController,
} from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'

type BackupType = 'create' | 'restore'

@Component({
  selector: 'backup-drives',
  templateUrl: './backup-drives.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesComponent {
  @Input() type: BackupType
  @Output() onSelect: EventEmitter<
    MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>
  > = new EventEmitter()
  loadingText: string

  constructor(
    private readonly loadingCtrl: LoadingController,
    private readonly actionCtrl: ActionSheetController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    public readonly backupService: BackupService,
  ) {}

  ngOnInit() {
    this.loadingText =
      this.type === 'create'
        ? 'Fetching Backup Targets'
        : 'Fetching Backup Sources'
    this.backupService.getBackupTargets()
  }

  select(
    target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>,
  ): void {
    if (target.entry.type === 'cifs' && !target.entry.mountable) {
      const message =
        'Unable to connect to LAN Shared Folder. Ensure (1) target computer is connected to LAN, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.'
      this.presentAlertError(message)
      return
    }

    if (this.type === 'restore' && !target.hasValidBackup) {
      const message = `${
        target.entry.type === 'cifs' ? 'LAN Shared Folder' : 'Drive partition'
      } does not contain a valid Embassy backup.`
      this.presentAlertError(message)
      return
    }

    this.onSelect.emit(target)
  }

  async presentModalAddCifs(): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'New LAN Shared Folder',
        spec: CifsSpec,
        buttons: [
          {
            text: 'Connect',
            handler: (value: RR.AddBackupTargetReq) => {
              return this.addCifs(value)
            },
            isSubmit: true,
          },
        ],
      },
    })
    await modal.present()
  }

  async presentActionCifs(
    event: Event,
    target: MappedBackupTarget<CifsBackupTarget>,
    index: number,
  ): Promise<void> {
    event.stopPropagation()

    const entry = target.entry as CifsBackupTarget

    const action = await this.actionCtrl.create({
      header: entry.hostname,
      subHeader: 'Shared Folder',
      mode: 'ios',
      buttons: [
        {
          text: 'Forget',
          icon: 'trash',
          role: 'destructive',
          handler: () => {
            this.deleteCifs(target.id, index)
          },
        },
        {
          text: 'Edit',
          icon: 'pencil',
          handler: () => {
            this.presentModalEditCifs(target.id, entry, index)
          },
        },
      ],
    })

    await action.present()
  }

  private async presentAlertError(message: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Error',
      message,
      buttons: ['OK'],
    })
    await alert.present()
  }

  private async addCifs(value: RR.AddBackupTargetReq): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      message: 'Testing connectivity to shared folder...',
    })
    await loader.present()

    try {
      const res = await this.embassyApi.addBackupTarget(value)
      const [id, entry] = Object.entries(res)[0]
      this.backupService.cifs.unshift({
        id,
        hasValidBackup: this.backupService.hasValidBackup(entry),
        entry,
      })
      return true
    } catch (e: any) {
      this.errToast.present(e)
      return false
    } finally {
      loader.dismiss()
    }
  }

  private async presentModalEditCifs(
    id: string,
    entry: CifsBackupTarget,
    index: number,
  ): Promise<void> {
    const { hostname, path, username } = entry

    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Update Shared Folder',
        spec: CifsSpec,
        buttons: [
          {
            text: 'Save',
            handler: (value: RR.AddBackupTargetReq) => {
              return this.editCifs({ id, ...value }, index)
            },
            isSubmit: true,
          },
        ],
        initialValue: {
          hostname,
          path,
          username,
        },
      },
    })
    await modal.present()
  }

  private async editCifs(
    value: RR.UpdateBackupTargetReq,
    index: number,
  ): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Testing connectivity to shared folder...',
    })
    await loader.present()

    try {
      const res = await this.embassyApi.updateBackupTarget(value)
      this.backupService.cifs[index].entry = Object.values(res)[0]
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async deleteCifs(id: string, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      message: 'Removing...',
    })
    await loader.present()

    try {
      await this.embassyApi.removeBackupTarget({ id })
      this.backupService.cifs.splice(index, 1)
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  refresh() {
    this.backupService.getBackupTargets()
  }
}

@Component({
  selector: 'backup-drives-header',
  templateUrl: './backup-drives-header.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesHeaderComponent {
  @Input() type: BackupType
  @Output() onClose: EventEmitter<void> = new EventEmitter()

  constructor(public readonly backupService: BackupService) {}

  refresh() {
    this.backupService.getBackupTargets()
  }
}

@Component({
  selector: 'backup-drives-status',
  templateUrl: './backup-drives-status.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesStatusComponent {
  @Input() type: string
  @Input() hasValidBackup: boolean
}

const CifsSpec: ConfigSpec = {
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
