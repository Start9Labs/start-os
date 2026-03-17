import { Component } from '@angular/core'
import { FormsModule } from '@angular/forms'
import { i18nPipe } from '@start9labs/shared'
import {
  TuiButton,
  TuiCheckbox,
  TuiDialogContext,
  TuiNotification,
  TuiTitle,
} from '@taiga-ui/core'
import { TuiHeader } from '@taiga-ui/layout'
import { injectContext, PolymorpheusComponent } from '@taiga-ui/polymorpheus'

export interface PreserveOverwriteData {
  isExt4: boolean
}

@Component({
  imports: [
    FormsModule,
    TuiButton,
    TuiCheckbox,
    TuiHeader,
    TuiNotification,
    TuiTitle,
    i18nPipe,
  ],
  template: `
    <header tuiHeader>
      <hgroup tuiTitle>
        <h2 [id]="context.id">{{ 'StartOS Data Detected' | i18n }}</h2>
        <p>{{ 'This drive contains existing StartOS data.' | i18n }}</p>
      </hgroup>
    </header>
    <ul>
      <li>
        <strong class="g-positive">{{ 'Preserve' | i18n }}</strong>
        {{ 'to keep your data.' | i18n }}
      </li>
      <li>
        <strong class="g-negative">{{ 'Overwrite' | i18n }}</strong>
        {{ 'to discard' | i18n }}
      </li>
    </ul>
    @if (context.data.isExt4) {
      <p tuiNotification appearance="warning" size="m">
        {{
          'This drive uses ext4 and will be automatically converted to btrfs. A backup is strongly recommended before proceeding.'
            | i18n
        }}
      </p>
      <label>
        <input tuiCheckbox type="checkbox" [(ngModel)]="backupAck" />
        {{ 'I have a backup of my data' | i18n }}
      </label>
    }
    <footer>
      <button
        tuiButton
        appearance="flat-destructive"
        (click)="context.completeWith(false)"
      >
        {{ 'Overwrite' | i18n }}
      </button>
      <button
        tuiButton
        appearance=""
        [style.background]="'var(--tui-status-positive)'"
        [disabled]="context.data.isExt4 && !backupAck"
        (click)="context.completeWith(true)"
      >
        {{ 'Preserve' | i18n }}
      </button>
    </footer>
  `,
})
export class PreserveOverwriteDialog {
  protected readonly context =
    injectContext<TuiDialogContext<boolean, PreserveOverwriteData>>()
  protected backupAck = false
}

export const PRESERVE_OVERWRITE = new PolymorpheusComponent(
  PreserveOverwriteDialog,
)
