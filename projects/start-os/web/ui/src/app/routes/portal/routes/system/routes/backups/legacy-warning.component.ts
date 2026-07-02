import { Component, inject, input } from '@angular/core'
import { DialogService, ErrorService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiNotificationService } from '@taiga-ui/core'
import { TuiNotificationMiddleService } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { BackupService } from './backup.service'

@Component({
  selector: 'backup-legacy-warning',
  template: `
    <button
      tuiButton
      size="s"
      iconStart="@tui.brush-cleaning"
      (click)="$event.stopPropagation(); remove()"
    >
      {{ 'Delete old backup' | i18n }}
    </button>
  `,
  styles: `
    :host {
      display: inline-flex;
    }
  `,
  imports: [TuiButton, i18nPipe],
})
export class BackupLegacyWarningComponent {
  private readonly dialog = inject(DialogService)
  private readonly alerts = inject(TuiNotificationService)
  private readonly loader = inject(TuiNotificationMiddleService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  private readonly service = inject(BackupService)
  private readonly i18n = inject(i18nPipe)

  readonly id = input.required<string>()

  remove() {
    this.dialog
      .openConfirm({
        label: 'Delete old backup?',
        size: 's',
        data: {
          content:
            'Permanently delete the old (V1) backup from this target? This cannot be undone. Your current (V2) backup will not be affected.',
          no: 'Cancel',
          yes: 'Delete',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(async () => {
        const loader = this.loader.open('Deleting old backup').subscribe()
        const id = this.id()

        try {
          await this.api.deleteLegacyBackup({ targetId: id })
          this.service.clearLegacy(id)
          this.alerts
            .open(this.i18n.transform('Old backup deleted'), {
              appearance: 'positive',
            })
            .subscribe()
        } catch (e: any) {
          this.errorService.handleError(e)
        } finally {
          loader.unsubscribe()
        }
      })
  }
}
