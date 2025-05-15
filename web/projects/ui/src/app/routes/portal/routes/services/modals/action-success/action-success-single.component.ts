import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  Input,
  ViewChild,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiButton } from '@taiga-ui/core'
import {
  TuiInputModule,
  TuiTextfieldComponent,
  TuiTextfieldControllerModule,
} from '@taiga-ui/legacy'
import { QrCodeModule } from 'ng-qrcode'
import { SingleResult } from './types'
import { i18nPipe } from '@start9labs/shared'

@Component({
  standalone: true,
  selector: 'app-action-success-single',
  template: `
    @if (single.qr) {
      <p class="qr"><ng-container *ngTemplateOutlet="qr" /></p>
    }
    <tui-input
      [readOnly]="true"
      [ngModel]="single.value"
      [tuiTextfieldLabelOutside]="true"
      [tuiTextfieldCustomContent]="actions"
    >
      <input
        tuiTextfieldLegacy
        [style.border-inline-end-width.rem]="border"
        [type]="single.masked && masked ? 'password' : 'text'"
      />
    </tui-input>
    <ng-template #actions>
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
          (click)="copy()"
        >
          {{ 'Copy' | i18n }}
        </button>
      }
    </ng-template>
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
  styles: [
    `
      @import '@taiga-ui/core/styles/taiga-ui-local';

      .reveal {
        @include center-all();
      }

      .qr {
        position: relative;
        text-align: center;
      }
    `,
  ],
  imports: [
    CommonModule,
    FormsModule,
    TuiInputModule,
    TuiTextfieldControllerModule,
    TuiButton,
    QrCodeModule,
    i18nPipe,
  ],
})
export class ActionSuccessSingleComponent {
  @ViewChild(TuiTextfieldComponent, { read: ElementRef })
  private readonly input!: ElementRef<HTMLInputElement>

  @Input()
  single!: SingleResult

  masked = true

  get border(): number {
    let border = 0

    if (this.single.masked) border += 2
    if (this.single.copyable) border += 2

    return border
  }

  copy() {
    const el = this.input.nativeElement

    if (!el) {
      return
    }

    el.type = 'text'
    el.focus()
    el.select()
    el.ownerDocument.execCommand('copy')
    el.type = this.masked && this.single.masked ? 'password' : 'text'
  }
}
