import { Component, Input } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { Subject } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import {
  BackupType,
  WithId,
} from '../../pages/backup-targets/backup-targets.page'
import { TargetService } from '../../services/target-service'

@Component({
  selector: 'target-select',
  templateUrl: './target-select.page.html',
  styleUrls: ['./target-select.page.scss'],
})
export class TargetSelectPage {
  @Input() type!: BackupType

  readonly targets$ = new Subject<WithId<BackupTarget>[]>()

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly api: ApiService,
    private readonly targetService: TargetService,
  ) {}

  ngOnInit() {
    this.getBackupTargets()
  }

  dismiss() {
    this.modalCtrl.dismiss()
  }

  select(targetId: string): void {
    this.modalCtrl.dismiss(targetId)
  }

  private async getBackupTargets(): Promise<WithId<BackupTarget>[]> {
    const targets = await this.api.getBackupTargets({})
    return Object.keys(targets).map(id => ({ id, ...targets[id] }))
  }
}

@Component({
  selector: 'target-status',
  templateUrl: './target-status.component.html',
  styleUrls: ['./target-select.page.scss'],
})
export class TargetStatusComponent {
  @Input() type!: BackupType
  @Input() hasValidBackup!: boolean
}
