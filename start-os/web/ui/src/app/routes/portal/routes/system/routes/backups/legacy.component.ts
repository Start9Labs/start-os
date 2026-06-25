import { Component } from '@angular/core'
import { i18nPipe } from '@start9labs/shared'
import {
  TuiButton,
  TuiDialogContext,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

export interface LegacyBackupData {
  fits: boolean
}

@Component({
  template: `
    <div tuiNotification appearance="warning" class="header">
      <span tuiTitle>
        <strong>{{ 'New Backup Format' | i18n }}</strong>
        <span tuiSubtitle>
          {{
            'A major performance improvement to StartOS backups has changed the backup format. The existing backup on this drive — the old "StartOSBackups" folder — is now obsolete and can no longer be updated. Your new backup will be created in a separate "StartOSBackupsV2" folder.'
              | i18n
          }}
        </span>
      </span>
    </div>

    @if (data.fits) {
      <div tuiNotification appearance="positive">
        <span tuiTitle>
          <strong>{{ 'Back up everything you need' | i18n }}</strong>
          <span tuiSubtitle>
            {{
              'Because the old backup will no longer be updated, be sure to select ANY and ALL services you want backed up. Once this new backup completes successfully, DELETE the old "StartOSBackups" folder from the drive to free up space — do NOT delete "StartOSBackupsV2".'
                | i18n
            }}
          </span>
        </span>
      </div>

      <footer class="g-buttons">
        <button
          tuiButton
          appearance="flat"
          (click)="context.completeWith(false)"
        >
          {{ 'Cancel' | i18n }}
        </button>
        <button tuiButton (click)="context.completeWith(true)">
          {{ 'Continue' | i18n }}
        </button>
      </footer>
    } @else {
      <div tuiNotification appearance="negative">
        <span tuiTitle>
          <strong>{{ 'Not enough free space for a new backup' | i18n }}</strong>
          <span tuiSubtitle>
            {{
              'The old "StartOSBackups" folder is larger than the free space remaining on this drive, so a new backup will not fit. You have two options:'
                | i18n
            }}
          </span>
        </span>
      </div>

      <div tuiNotification appearance="positive">
        <div class="options">
          <p>
            <strong>{{ 'Option 1' | i18n }}:</strong>
            {{
              'Delete the old "StartOSBackups" folder from this drive, then try again.'
                | i18n
            }}
          </p>
          <p>
            <strong>{{ 'Option 2' | i18n }}:</strong>
            {{ 'Choose a different drive for this backup.' | i18n }}
          </p>
        </div>
      </div>

      <footer class="g-buttons">
        <button tuiButton (click)="context.completeWith(false)">
          {{ 'OK' | i18n }}
        </button>
      </footer>
    }
  `,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      gap: 1rem;
    }

    .header strong {
      font-size: 1.1rem;
    }

    .options {
      display: flex;
      flex-direction: column;
      gap: 0.5rem;

      p {
        margin: 0;
      }
    }

    footer {
      margin-top: 0.5rem;
    }
  `,
  imports: [TuiButton, TuiNotification, TuiTitle, i18nPipe],
})
export class LegacyBackupModal {
  readonly context =
    injectContext<TuiDialogContext<boolean, LegacyBackupData>>()
  readonly data = this.context.data
}

export const LEGACY_BACKUP = new PolymorpheusComponent(LegacyBackupModal)
