import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { CopyService, i18nPipe } from '@start9labs/shared'
import { TuiButton, TuiInput } from '@taiga-ui/core'
import { QrCodeComponent } from 'ng-qrcode'
import { SingleResult } from './types'

@Component({
  selector: 'app-action-success-single',
  template: `
    @if (single.qr) {
      <p class="qr"><ng-container *ngTemplateOutlet="qr" /></p>
    }
    <tui-textfield>
      <input
        tuiInput
        [readOnly]="true"
        [ngModel]="single.value"
        [type]="single.masked && masked ? 'password' : 'text'"
      />
      @if (single.masked) {
        <button
          tuiIconButton
          appearance="icon"
          size="s"
          type="button"
          tabindex="-1"
          [iconStart]="masked ? '@tui.eye' : '@tui.eye-off'"
          [style.pointer-events]="'auto'"
          (click)="masked = !masked"
        >
          {{ 'Reveal/Hide' | i18n }}
        </button>
      }
      @if (single.copyable) {
        <button
          tuiIconButton
          appearance="icon"
          size="s"
          type="button"
          tabindex="-1"
          iconStart="@tui.copy"
          [style.pointer-events]="'auto'"
          (click)="copy.copy(single.value)"
        >
          {{ 'Copy' | i18n }}
        </button>
      }
    </tui-textfield>
    <ng-template #qr>
      <qr-code
        [value]="single.value"
        [style.filter]="single.masked && masked ? 'blur(0.5rem)' : null"
        size="350"
      />
      @if (single.masked && masked) {
        <button
          tuiIconButton
          class="reveal"
          iconStart="@tui.eye"
          [style.border-radius.%]="100"
          (click)="masked = false"
        >
          {{ 'Reveal' | i18n }}
        </button>
      }
    </ng-template>
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: `
    @use '@taiga-ui/styles/utils' as taiga;

    .reveal {
      @include taiga.center-all();
    }

    .qr {
      position: relative;
      text-align: center;
    }
  `,
  imports: [
    CommonModule,
    FormsModule,
    TuiInput,
    TuiButton,
    QrCodeComponent,
    i18nPipe,
  ],
})
export class ActionSuccessSingleComponent {
  readonly copy = inject(CopyService)

  @Input()
  single!: SingleResult

  masked = true
}
