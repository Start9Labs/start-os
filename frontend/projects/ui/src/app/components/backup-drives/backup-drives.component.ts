import { Component, EventEmitter, Input, Output } from '@angular/core'
import { BackupService } from './backup.service'
import { CifsBackupTarget, DiskBackupTarget, RR } from 'src/app/services/api/api.types'
import { ActionSheetController, AlertController, LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { ConfigSpec } from 'src/app/pkg-config/config-types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'
import { MappedBackupTarget } from 'src/app/util/misc.util'

@Component({
  selector: 'backup-drives',
  templateUrl: './backup-drives.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesComponent {
  @Input() type: 'create' | 'restore'
  @Output() onSelect: EventEmitter<MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>> = new EventEmitter()
  loadingText: string

  constructor (
    private readonly loadingCtrl: LoadingController,
    private readonly actionCtrl: ActionSheetController,
    private readonly alertCtrl: AlertController,
    private readonly modalCtrl: ModalController,
    private readonly embassyApi: ApiService,
    private readonly errToast: ErrorToastService,
    public readonly backupService: BackupService,
  ) { }

  ngOnInit () {
    this.loadingText = this.type === 'create' ? 'Fetching Backup Targets' : 'Fetching Backup Sources'
    this.backupService.getBackupTargets()
  }

  select (target: MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>): void {
    if (target.entry.type === 'cifs' && !target.entry.mountable) {
      const message = 'Unable to connect to shared folder. Ensure (1) target computer is connected to LAN, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.'
      this.presentAlertError(message)
      return
    }

    if (this.type === 'restore' && !target.hasValidBackup) {
      const message = `${target.entry.type === 'cifs' ? 'Shared folder' : 'Drive partition'} does not contain a valid Embassy backup.`
      this.presentAlertError(message)
      return
    }

    this.onSelect.emit(target)
  }

  async presentModalAddCifs (): Promise<void> {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'New Shared Folder',
        spec: CifsSpec,
        buttons: [
          {
            text: 'Save',
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

  async presentActionCifs (target: MappedBackupTarget<CifsBackupTarget>, index: number): Promise<void> {
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
        {
          text: this.type === 'create' ? 'Create Backup' : 'Restore From Backup',
          icon: this.type === 'create' ? 'cloud-upload-outline' : 'cloud-download-outline',
          handler: () => {
            this.select(target)
          },
        },
      ],
    })

    await action.present()
  }

  private async presentAlertError (message: string): Promise<void> {
    const alert = await this.alertCtrl.create({
      header: 'Error',
      message,
      buttons: ['OK'],
    })
    await alert.present()
  }

  private async addCifs (value: RR.AddBackupTargetReq): Promise<boolean> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Testing connectivity to shared folder...',
      cssClass: 'loader',
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
    } catch (e) {
      this.errToast.present(e)
      return false
    } finally {
      loader.dismiss()
    }
  }

  private async presentModalEditCifs (id: string, entry: CifsBackupTarget, index: number): Promise<void> {
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

  private async editCifs (value: RR.UpdateBackupTargetReq, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Testing connectivity to shared folder...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      const res = await this.embassyApi.updateBackupTarget(value)
      const entry = Object.values(res)[0]
      this.backupService.cifs[index].entry = entry
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }

  private async deleteCifs (id: string, index: number): Promise<void> {
    const loader = await this.loadingCtrl.create({
      spinner: 'lines',
      message: 'Removing...',
      cssClass: 'loader',
    })
    await loader.present()

    try {
      await this.embassyApi.removeBackupTarget({ id })
      this.backupService.cifs.splice(index, 1)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}


@Component({
  selector: 'backup-drives-header',
  templateUrl: './backup-drives-header.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesHeaderComponent {
  @Input() title: string
  @Output() onClose: EventEmitter<void> = new EventEmitter()

  constructor (
    public readonly backupService: BackupService,
  ) { }

  refresh () {
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
    description: 'The hostname of your target device on the Local Area Network.',
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
    description: 'The directory path to the shared folder on your target device.',
    placeholder: 'e.g. /Desktop/my-folder',
    nullable: false,
    masked: false,
    copyable: false,
  },
  username: {
    type: 'string',
    name: 'Username',
    description: 'The username of the user account on your target device.',
    nullable: false,
    masked: false,
    copyable: false,
  },
  password: {
    type: 'string',
    name: 'Password',
    description: 'The password of the user account on your target device.',
    nullable: true,
    masked: true,
    copyable: false,
  },
}
