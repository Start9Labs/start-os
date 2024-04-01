import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiDialogContext, TuiGroupModule } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiCheckboxBlockModule } from '@taiga-ui/kit'
import {
  POLYMORPHEUS_CONTEXT,
  PolymorpheusComponent,
} from '@tinkoff/ng-polymorpheus'
import { PatchDB } from 'patch-db-client'
import { take } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { PackageBackupInfo } from 'src/app/services/api/api.types'
import { ToOptionsPipe } from '../pipes/to-options.pipe'
import { RecoverOption } from '../types/recover-option'
import { RecoverData } from '../types/recover-data'
import { TuiMapperPipeModule } from '@taiga-ui/cdk'

@Component({
  template: `
    <ng-container *ngIf="packageData$ | toOptions: backups | async as options">
      <div tuiGroup orientation="vertical" [style.width.%]="100">
        <tui-checkbox-block
          *ngFor="let option of options"
          [disabled]="option.installed || option.newerStartOs"
          [(ngModel)]="option.checked"
        >
          <div [style.margin]="'0.75rem 0'">
            <strong>{{ option.title }}</strong>
            <div>Version {{ option.version }}</div>
            <div>Backup made: {{ option.timestamp | date: 'medium' }}</div>
            <div
              *ngIf="option | tuiMapper: toMessage as message"
              [style.color]="message.color"
            >
              {{ message.text }}
            </div>
          </div>
        </tui-checkbox-block>
      </div>

      <footer class="g-buttons">
        <button
          tuiButton
          [disabled]="isDisabled(options)"
          (click)="restore(options)"
        >
          Restore Selected
        </button>
      </footer>
    </ng-container>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    ToOptionsPipe,
    TuiButtonModule,
    TuiCheckboxBlockModule,
    TuiGroupModule,
    TuiMapperPipeModule,
  ],
})
export class BackupsRecoverModal {
  private readonly api = inject(ApiService)
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly context =
    inject<TuiDialogContext<void, RecoverData>>(POLYMORPHEUS_CONTEXT)

  readonly packageData$ = inject(PatchDB<DataModel>)
    .watch$('packageData')
    .pipe(take(1))

  readonly toMessage = (option: RecoverOption) => {
    if (option.newerStartOs) {
      return {
        text: `Unavailable. Backup was made on a newer version of StartOS.`,
        color: 'var(--tui-error-fill)',
      }
    }

    if (option.installed) {
      return {
        text: `Unavailable. ${option.title} is already installed.`,
        color: 'var(--tui-warning-fill)',
      }
    }

    return {
      text: 'Ready to restore',
      color: 'var(--tui-success-fill)',
    }
  }

  get backups(): Record<string, PackageBackupInfo> {
    return this.context.data.backupInfo.packageBackups
  }

  isDisabled(options: RecoverOption[]): boolean {
    return options.every(o => !o.checked)
  }

  async restore(options: RecoverOption[]): Promise<void> {
    const ids = options.filter(({ checked }) => !!checked).map(({ id }) => id)
    const loader = this.loader.open('Initializing...').subscribe()

    const { targetId, password } = this.context.data

    try {
      await this.api.restorePackages({
        ids,
        targetId,
        password,
      })

      this.context.$implicit.complete()
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}

export const RECOVER = new PolymorpheusComponent(BackupsRecoverModal)
