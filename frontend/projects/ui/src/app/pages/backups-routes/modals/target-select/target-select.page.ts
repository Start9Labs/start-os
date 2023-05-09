import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { BehaviorSubject } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
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

  targets: BackupTarget[] = []

  loading$ = new BehaviorSubject(true)

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
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
    await this.getTargets()
  }

  private async getTargets(): Promise<void> {
    this.loading$.next(true)
    try {
      this.targets = (await this.api.getBackupTargets({})).saved
    } catch (e: any) {
      this.errToast.present(e)
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
