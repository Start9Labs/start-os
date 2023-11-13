import { Component, Input } from '@angular/core'
import { NavController } from '@ionic/angular'
import { CifsModal } from 'src/app/modals/cifs-modal/cifs-modal.page'
import {
  ApiService,
  CifsRecoverySource,
  DiskBackupTarget,
} from 'src/app/services/api/api.service'
import { ErrorService } from '@start9labs/shared'
import { StateService } from 'src/app/services/state.service'
import { PASSWORD } from '../../modals/password/password.page'
import { TuiDialogService } from '@taiga-ui/core'
import { filter } from 'rxjs'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'app-recover',
  templateUrl: 'recover.page.html',
  styleUrls: ['recover.page.scss'],
})
export class RecoverPage {
  loading = true
  mappedDrives: MappedDisk[] = []

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly dialogs: TuiDialogService,
    private readonly errorService: ErrorService,
    private readonly stateService: StateService,
  ) {}

  async ngOnInit() {
    this.stateService.setupType = 'restore'
    await this.getDrives()
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  driveClickable(mapped: MappedDisk) {
    return mapped.drive['embassy-os']?.full
  }

  async getDrives() {
    this.mappedDrives = []
    try {
      const disks = await this.apiService.getDrives()
      disks
        .filter(d => d.partitions.length)
        .forEach(d => {
          d.partitions.forEach(p => {
            const drive: DiskBackupTarget = {
              vendor: d.vendor,
              model: d.model,
              logicalname: p.logicalname,
              label: p.label,
              capacity: p.capacity,
              used: p.used,
              'embassy-os': p['embassy-os'],
            }
            this.mappedDrives.push({
              hasValidBackup: !!p['embassy-os']?.full,
              drive,
            })
          })
        })
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  presentModalCifs() {
    this.dialogs
      .open<{ cifs: CifsRecoverySource; recoveryPassword: string }>(
        new PolymorpheusComponent(CifsModal),
        {
          label: 'Connect Network Folder',
        },
      )
      .subscribe(({ cifs, recoveryPassword }) => {
        this.stateService.recoverySource = {
          type: 'backup',
          target: cifs,
        }
        this.stateService.recoveryPassword = recoveryPassword
        this.navCtrl.navigateForward('/storage')
      })
  }

  async select(target: DiskBackupTarget) {
    const { logicalname } = target

    if (!logicalname) return

    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Unlock Drive',
        size: 's',
        data: { target },
      })
      .pipe(filter(Boolean))
      .subscribe(password => {
        this.selectRecoverySource(logicalname, password)
      })
  }

  private async selectRecoverySource(logicalname: string, password?: string) {
    this.stateService.recoverySource = {
      type: 'backup',
      target: {
        type: 'disk',
        logicalname,
      },
    }
    this.stateService.recoveryPassword = password
    this.navCtrl.navigateForward(`/storage`)
  }
}

@Component({
  selector: 'drive-status',
  templateUrl: './drive-status.component.html',
  styleUrls: ['./recover.page.scss'],
})
export class DriveStatusComponent {
  @Input({ required: true }) hasValidBackup!: boolean
}

interface MappedDisk {
  hasValidBackup: boolean
  drive: DiskBackupTarget
}
