import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import { ModalController, NavController } from '@ionic/angular'
import { BehaviorSubject } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService } from '@start9labs/shared'
import { BackupType } from '../../pages/backup-targets/backup-targets.page'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { TuiDialogContext } from '@taiga-ui/core'

@Component({
  selector: 'target-select',
  templateUrl: './target-select.page.html',
  styleUrls: ['./target-select.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TargetSelectPage {
  // TODO: What is it for?
  @Input() isOneOff = true

  targets: BackupTarget[] = []

  loading$ = new BehaviorSubject(true)

  constructor(
    @Inject(POLYMORPHEUS_CONTEXT)
    private readonly context: TuiDialogContext<
      BackupTarget,
      { type: BackupType }
    >,
    private readonly modalCtrl: ModalController,
    private readonly navCtrl: NavController,
    private readonly api: ApiService,
    private readonly errorService: ErrorService,
  ) {}

  get type(): BackupType {
    return this.context.data.type
  }

  async ngOnInit() {
    await this.getTargets()
  }

  select(target: BackupTarget): void {
    this.context.completeWith(target)
  }

  goToTargets() {
    this.context.$implicit.complete()
    this.navCtrl.navigateForward(`/backups/targets`)
  }

  async refresh() {
    await this.getTargets()
  }

  private async getTargets(): Promise<void> {
    this.loading$.next(true)
    try {
      this.targets = (await this.api.getBackupTargets({})).saved
    } catch (e: any) {
      this.errorService.handleError(e)
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
