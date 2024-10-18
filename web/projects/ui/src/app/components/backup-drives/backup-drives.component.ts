import { Component, EventEmitter, Input, Output } from '@angular/core'
import { ActionSheetController, AlertController } from '@ionic/angular'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { ISB } from '@start9labs/start-sdk'
import {
  CifsBackupTarget,
  DiskBackupTarget,
  RR,
} from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { MappedBackupTarget } from 'src/app/types/mapped-backup-target'
import { configBuilderToSpec } from 'src/app/util/configBuilderToSpec'
import { FormComponent } from '../form.component'
import { BackupService } from './backup.service'

type BackupType = 'create' | 'restore'

@Component({
  selector: 'backup-drives',
  templateUrl: './backup-drives.component.html',
  styleUrls: ['./backup-drives.component.scss'],
})
export class BackupDrivesComponent {
  @Input() type!: BackupType
  @Output() onSelect: EventEmitter<
    MappedBackupTarget<CifsBackupTarget | DiskBackupTarget>
  > = new EventEmitter()
  loadingText = ''

  constructor(
    private readonly loader: LoadingService,
    private readonly actionCtrl: ActionSheetController,
    private readonly alertCtrl: AlertController,
    private readonly embassyApi: ApiService,
    private readonly errorService: ErrorService,
    private readonly backupService: BackupService,
    private readonly formDialog: FormDialogService,
  ) {}

  get loading() {
    return this.backupService.loading
  }

  get loadingError() {
    return this.backupService.loadingError
  }

  get drives() {
    return this.backupService.drives
  }

  get cifs() {
    return this.backupService.cifs
  }

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
        'Unable to connect to Network Folder. Ensure (1) target computer is connected to the same LAN as your Start9 Server, (2) target folder is being shared, and (3) hostname, path, and credentials are accurate.'
      this.presentAlertError(message)
      return
    }

    if (this.type === 'restore' && !target.hasAnyBackup) {
      const message = `${
        target.entry.type === 'cifs' ? 'Network Folder' : 'Drive partition'
      } does not contain a valid backup.`
      this.presentAlertError(message)
      return
    }

    this.onSelect.emit(target)
  }

  async presentModalAddCifs(): Promise<void> {
    this.formDialog.open(FormComponent, {
      label: 'New Network Folder',
      data: {
        spec: await configBuilderToSpec(cifsSpec),
        buttons: [
          {
            text: 'Execute',
            handler: async (value: RR.AddBackupTargetReq) =>
              this.addCifs(value),
          },
        ],
      },
    })
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
    const loader = this.loader
      .open('Testing connectivity to shared folder...')
      .subscribe()

    try {
      const res = await this.embassyApi.addBackupTarget(value)
      const [id, entry] = Object.entries(res)[0]
      this.backupService.cifs.unshift({
        id,
        hasAnyBackup: this.backupService.hasAnyBackup(entry),
        entry,
      })
      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async presentModalEditCifs(
    id: string,
    entry: CifsBackupTarget,
    index: number,
  ): Promise<void> {
    const { hostname, path, username } = entry

    this.formDialog.open(FormComponent, {
      label: 'Update Network Folder',
      data: {
        spec: await configBuilderToSpec(cifsSpec),
        buttons: [
          {
            text: 'Execute',
            handler: async (value: RR.AddBackupTargetReq) =>
              this.editCifs({ id, ...value }, index),
          },
        ],
        value: {
          hostname,
          path,
          username,
        },
      },
    })
  }

  private async editCifs(
    value: RR.UpdateBackupTargetReq,
    index: number,
  ): Promise<boolean> {
    const loader = this.loader
      .open('Testing connectivity to shared folder...')
      .subscribe()

    try {
      const res = await this.embassyApi.updateBackupTarget(value)
      this.backupService.cifs[index].entry = Object.values(res)[0]

      return true
    } catch (e: any) {
      this.errorService.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }

  private async deleteCifs(id: string, index: number): Promise<void> {
    const loader = this.loader.open('Removing...').subscribe()

    try {
      await this.embassyApi.removeBackupTarget({ id })
      this.backupService.cifs.splice(index, 1)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
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
  @Input() type!: BackupType
  @Output() onClose: EventEmitter<void> = new EventEmitter()

  constructor(private readonly backupService: BackupService) {}

  get loading() {
    return this.backupService.loading
  }

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
  @Input() type!: BackupType
  @Input() hasAnyBackup!: boolean
}

const cifsSpec = ISB.InputSpec.of({
  hostname: ISB.Value.text({
    name: 'Hostname',
    description:
      'The hostname of your target device on the Local Area Network.',
    warning: null,
    placeholder: `e.g. 'My Computer' OR 'my-computer.local'`,
    required: { default: null },
    patterns: [],
  }),
  path: ISB.Value.text({
    name: 'Path',
    description: `On Windows, this is the fully qualified path to the shared folder, (e.g. /Desktop/my-folder).\n\n On Linux and Mac, this is the literal name of the shared folder (e.g. my-shared-folder).`,
    placeholder: 'e.g. my-shared-folder or /Desktop/my-folder',
    required: { default: null },
  }),
  username: ISB.Value.text({
    name: 'Username',
    description: `On Linux, this is the samba username you created when sharing the folder.\n\n On Mac and Windows, this is the username of the user who is sharing the folder.`,
    required: { default: null },
    placeholder: 'My Network Folder',
  }),
  password: ISB.Value.text({
    name: 'Password',
    description: `On Linux, this is the samba password you created when sharing the folder.\n\n On Mac and Windows, this is the password of the user who is sharing the folder.`,
    required: false,
    masked: true,
    placeholder: 'My Network Folder',
  }),
})
