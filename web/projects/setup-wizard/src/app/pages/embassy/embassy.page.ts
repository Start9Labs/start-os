import { Component } from '@angular/core'
import { NavController } from '@ionic/angular'
import {
  DiskInfo,
  ErrorService,
  GuidPipe,
  LoadingService,
} from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import {
  ApiService,
  BackupRecoverySource,
  DiskRecoverySource,
  DiskMigrateSource,
} from 'src/app/services/api/api.service'
import { StateService } from 'src/app/services/state.service'
import { PASSWORD, PasswordPage } from '../../modals/password/password.page'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter, of, switchMap } from 'rxjs'

@Component({
  selector: 'app-embassy',
  templateUrl: 'embassy.page.html',
  styleUrls: ['embassy.page.scss'],
  providers: [GuidPipe],
})
export class EmbassyPage {
  storageDrives: DiskInfo[] = []
  loading = true

  constructor(
    private readonly apiService: ApiService,
    private readonly navCtrl: NavController,
    private readonly dialogs: TuiDialogService,
    private readonly stateService: StateService,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly guidPipe: GuidPipe,
  ) {}

  async ngOnInit() {
    await this.getDrives()
  }

  tooSmall(drive: DiskInfo) {
    return drive.capacity < 34359738368
  }

  async refresh() {
    this.loading = true
    await this.getDrives()
  }

  async getDrives() {
    this.loading = true
    try {
      const disks = await this.apiService.getDrives()
      if (this.stateService.setupType === 'fresh') {
        this.storageDrives = disks
      } else if (this.stateService.setupType === 'restore') {
        this.storageDrives = disks.filter(
          d =>
            !d.partitions
              .map(p => p.logicalname)
              .includes(
                (
                  (this.stateService.recoverySource as BackupRecoverySource)
                    ?.target as DiskRecoverySource
                )?.logicalname,
              ),
        )
      } else if (this.stateService.setupType === 'transfer') {
        const guid = (this.stateService.recoverySource as DiskMigrateSource)
          .guid
        this.storageDrives = disks.filter(d => {
          return (
            d.guid !== guid && !d.partitions.map(p => p.guid).includes(guid)
          )
        })
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }

  chooseDrive(drive: DiskInfo) {
    of(!this.guidPipe.transform(drive) && !drive.partitions.some(p => p.used))
      .pipe(
        switchMap(unused =>
          unused
            ? of(true)
            : this.dialogs.open(TUI_PROMPT, {
                label: 'Warning',
                size: 's',
                data: {
                  content:
                    '<strong>Drive contains data!</strong><p>All data stored on this drive will be permanently deleted.</p>',
                  yes: 'Continue',
                  no: 'Cancel',
                },
              }),
        ),
      )
      .pipe(filter(Boolean))
      .subscribe(() => {
        // for backup recoveries
        if (this.stateService.recoveryPassword) {
          this.setupEmbassy(
            drive.logicalname,
            this.stateService.recoveryPassword,
          )
        } else {
          // for migrations and fresh setups
          this.presentModalPassword(drive.logicalname)
        }
      })
  }

  private presentModalPassword(logicalname: string) {
    this.dialogs
      .open<string>(PASSWORD, {
        label: 'Set Password',
        size: 's',
        data: { storageDrive: true },
      })
      .subscribe(password => {
        this.setupEmbassy(logicalname, password)
      })
  }

  private async setupEmbassy(
    logicalname: string,
    password: string,
  ): Promise<void> {
    const loader = this.loader.open('Connecting to drive...').subscribe()

    try {
      await this.stateService.setupEmbassy(logicalname, password)
      await this.navCtrl.navigateForward(`/loading`)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
