import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { ErrorService } from '@start9labs/shared'
import { TuiForModule } from '@taiga-ui/cdk'
import { TuiButtonModule } from '@taiga-ui/experimental'
import {
  TuiDialogContext,
  TuiDialogOptions,
  TuiDialogService,
  TuiLoaderModule,
  TuiSvgModule,
} from '@taiga-ui/core'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { BehaviorSubject } from 'rxjs'
import { BackupTarget } from 'src/app/services/api/api.types'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupType } from '../types/backup-type'
import { BackupsStatusComponent } from '../components/status.component'
import { GetDisplayInfoPipe } from '../pipes/get-display-info.pipe'
import { TARGETS } from './targets.component'

@Component({
  template: `
    <tui-loader
      *ngIf="loading$ | async; else loaded"
      size="l"
      [textContent]="loading"
    ></tui-loader>
    <ng-template #loaded>
      <h3 class="g-title">Saved Targets</h3>
      <button
        *ngFor="let target of targets; empty: blank"
        class="g-action"
        [disabled]="isDisabled(target)"
        (click)="context.completeWith(target)"
      >
        <ng-container *ngIf="target | getDisplayInfo as displayInfo">
          <tui-svg [src]="displayInfo.icon"></tui-svg>
          <div>
            <strong>{{ displayInfo.name }}</strong>
            <backups-status
              [type]="context.data.type"
              [target]="target"
            ></backups-status>
            <div [style.color]="'var(--tui-text-02'">
              {{ displayInfo.description }}
              <br />
              {{ displayInfo.path }}
            </div>
          </div>
        </ng-container>
      </button>
      <ng-template #blank>
        <p>No saved targets</p>
        <button tuiButton (click)="goToTargets()">Go to Targets</button>
      </ng-template>
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    TuiLoaderModule,
    TuiForModule,
    TuiButtonModule,
    TuiSvgModule,
    BackupsStatusComponent,
    GetDisplayInfoPipe,
  ],
})
export class BackupsTargetModal {
  private readonly dialogs = inject(TuiDialogService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly context =
    inject<TuiDialogContext<BackupTarget, { type: BackupType }>>(
      POLYMORPHEUS_CONTEXT,
    )

  readonly loading$ = new BehaviorSubject(true)
  readonly loading =
    this.context.data.type === 'create'
      ? 'Loading Backup Targets'
      : 'Loading Backup Sources'

  targets: BackupTarget[] = []

  async ngOnInit() {
    try {
      this.targets = (await this.api.getBackupTargets({})).saved
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      this.loading$.next(false)
    }
  }

  isDisabled(target: BackupTarget): boolean {
    return (
      !target.mountable ||
      (this.context.data.type === 'restore' && !target.startOs)
    )
  }

  goToTargets() {
    this.context.$implicit.complete()
    this.dialogs
      .open(TARGETS, { label: 'Backup Targets', size: 'l' })
      .subscribe()
  }
}

export const TARGET = new PolymorpheusComponent(BackupsTargetModal)

export const TARGET_CREATE: Partial<TuiDialogOptions<{ type: BackupType }>> = {
  label: 'Select Backup Target',
  data: { type: 'create' },
}

export const TARGET_RESTORE: Partial<TuiDialogOptions<{ type: BackupType }>> = {
  label: 'Select Backup Source',
  data: { type: 'restore' },
}
