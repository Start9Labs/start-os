import {
  ChangeDetectionStrategy,
  Component,
  inject,
  signal,
} from '@angular/core'
import { TuiResponsiveDialogService } from '@taiga-ui/addon-mobile'
import { TuiButton } from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { TuiCardLarge } from '@taiga-ui/layout'
import { filter } from 'rxjs'
import { ActionService } from 'src/app/services/action.service'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  template: `
    <section class="g-form" tuiCardLarge>
      <h3>Create Backup</h3>
      <p>
        Download a backup of all router settings, including security profiles,
        WiFi configuration, SSL certificates, and system preferences.
      </p>
      <button
        tuiButton
        appearance="outline"
        iconStart="@tui.download"
        (click)="download()"
      >
        Download Backup
      </button>
    </section>
    <section class="g-form" tuiCardLarge>
      <h3>Restore Backup</h3>
      <p>
        Upload a previously created backup to restore all settings. The device
        will reboot after restoring.
      </p>
      <input
        #fileInput
        type="file"
        accept=".tar.gz,.gz"
        (change)="onFileSelected($event)"
        style="display: none"
      />
      <div class="restore-row">
        <button
          tuiButton
          appearance="outline"
          iconStart="@tui.file"
          (click)="fileInput.click()"
        >
          {{ selectedFile() ? selectedFile()!.name : 'Choose File' }}
        </button>
        <button
          tuiButton
          iconStart="@tui.hard-drive-upload"
          [disabled]="!selectedFile() || uploading()"
          (click)="restore()"
        >
          {{ uploading() ? 'Restoring...' : 'Restore' }}
        </button>
      </div>
    </section>
  `,
  styles: `
    section {
      align-items: start;
    }

    h3 {
      margin: 0;
    }

    p {
      margin: 0;
      color: var(--tui-text-secondary);
    }

    .restore-row {
      display: flex;
      gap: 1rem;
      align-items: center;
    }
  `,
  host: { class: 'g-page' },
  imports: [TuiCardLarge, TuiButton],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export default class Backup {
  private readonly api = inject(ApiService)
  private readonly actions = inject(ActionService)
  private readonly dialogs = inject(TuiResponsiveDialogService)

  protected readonly uploading = signal(false)
  protected readonly selectedFile = signal<File | null>(null)

  protected onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement
    this.selectedFile.set(input.files?.[0] ?? null)
  }

  protected async download() {
    await this.actions.run(
      async () => {
        const { guid, filename } = await this.api.backupCreate()
        const res = await fetch(`/rest/rpc/${guid}`)
        if (!res.ok) throw new Error(`Download failed: ${res.statusText}`)
        const blob = await res.blob()
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = filename
        a.click()
        URL.revokeObjectURL(url)
      },
      { loading: 'Creating backup...', success: 'Backup downloaded' },
    )
  }

  protected restore() {
    this.dialogs
      .open(TUI_CONFIRM, {
        label: 'Restore Backup',
        data: {
          content:
            'All current settings will be overwritten and the device will reboot. Are you sure you want to continue?',
          yes: 'Restore & Reboot',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean))
      .subscribe(() => this.doRestore())
  }

  private async doRestore() {
    const file = this.selectedFile()
    if (!file) return

    this.uploading.set(true)
    const success = await this.actions.run(
      async () => {
        const { upload } = await this.api.backupRestore()
        const arrayBuffer = await file.arrayBuffer()
        const res = await fetch(`/rest/rpc/${upload}`, {
          method: 'POST',
          body: arrayBuffer,
        })
        if (!res.ok) {
          const text = await res.text()
          throw new Error(text || res.statusText)
        }
        // Device is rebooting — poll until it goes down
        while (true) {
          await this.api.systemInfo()
        }
      },
      {
        loading: 'Restoring backup...',
        success: 'Backup restored',
        restart: true,
      },
    )
    this.uploading.set(false)
    if (success) this.selectedFile.set(null)
  }
}
