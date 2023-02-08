import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { BehaviorSubject, Subject } from 'rxjs'
import { BackupTarget, DiskBackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupType } from '../../pages/backup-targets/backup-targets.page'

@Component({
  selector: 'target-select',
  templateUrl: './target-select.page.html',
  styleUrls: ['./target-select.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TargetSelectPage {
  @Input() type!: BackupType
  @Input() isOneOff = true

  targets: {
    'unsaved-physical': DiskBackupTarget[]
    saved: BackupTarget[]
  } = {
    'unsaved-physical': [],
    saved: [],
  }

  loading$ = new BehaviorSubject(true)
  error$ = new Subject<string>()

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly api: ApiService,
  ) {}

  async ngOnInit() {
    await this.getTargets()
  }

  dismiss() {
    this.modalCtrl.dismiss()
  }

  select(target: BackupTarget): void {
    this.modalCtrl.dismiss(target)
  }

  goToTargets() {
    this.modalCtrl
      .dismiss()
      .then(() => this.navCtrl.navigateForward(`/backups/targets`))
  }

  async refresh() {
    this.loading$.next(true)
    this.error$.next('')
    await this.getTargets()
  }

  private async getTargets(): Promise<void> {
    try {
      const targets = await this.api.getBackupTargets({})
      this.targets = {
        'unsaved-physical': [],
        saved: targets,
      }
    } catch (e: any) {
      this.error$.next(e.message)
    } finally {
      this.loading$.next(false)
    }
  }
}

@Component({
  selector: 'target-status',
  templateUrl: './target-status.component.html',
  styleUrls: ['./target-select.page.scss'],
})
export class TargetStatusComponent {
  @Input() type!: BackupType
  @Input() target!: BackupTarget
}
