import {
  ChangeDetectionStrategy,
  Component,
  ElementRef,
  inject,
  Input,
  TemplateRef,
  ViewChild,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { T } from '@start9labs/start-sdk'
import { TuiButton, TuiDialogService, TuiTitle } from '@taiga-ui/core'
import {
  TuiInputModule,
  TuiTextfieldComponent,
  TuiTextfieldControllerModule,
} from '@taiga-ui/legacy'
import { QrCodeModule } from 'ng-qrcode'

@Component({
  standalone: true,
  selector: 'app-action-success-member',
  template: `
    <tui-input
      [readOnly]="true"
      [ngModel]="member.value"
      [tuiTextfieldCustomContent]="actions"
    >
      {{ member.name }}
      <input
        tuiTextfieldLegacy
        [style.border-inline-end-width.rem]="border"
        [type]="member.masked && masked ? 'password' : 'text'"
      />
    </tui-input>
    @if (member.description) {
      <label [style.padding-top.rem]="0.25" tuiTitle>
        <span tuiSubtitle [style.opacity]="0.8">{{ member.description }}</span>
      </label>
    }
    <ng-template #actions>
      @if (member.masked) {
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
          Reveal/Hide
        </button>
      }
      @if (member.copyable) {
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
          Copy
        </button>
      }
      @if (member.qr) {
        <button
          tuiIconButton
          appearance="icon"
          size="s"
          type="button"
          tabindex="-1"
          iconStart="@tui.qr-code"
          [style.pointer-events]="'auto'"
          (click)="show(qr)"
        >
          Show QR
        </button>
      }
    </ng-template>
    <ng-template #qr>
      <qr-code
        [value]="member.value"
        [style.filter]="member.masked && masked ? 'blur(0.5rem)' : null"
        size="350"
      />
      @if (member.masked && masked) {
        <button
          tuiIconButton
          class="reveal"
          iconStart="@tui.eye"
          [style.border-radius.%]="100"
          (click)="masked = false"
        >
          Reveal
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
    FormsModule,
    TuiInputModule,
    TuiTextfieldControllerModule,
    TuiButton,
    QrCodeModule,
    TuiTitle,
  ],
})
export class ActionSuccessMemberComponent {
  @ViewChild(TuiTextfieldComponent, { read: ElementRef })
  private readonly input!: ElementRef<HTMLInputElement>
  private readonly dialogs = inject(TuiDialogService)

  @Input()
  member!: T.ActionResultMember & { type: 'single' }

  masked = true

  get border(): number {
    let border = 0

    if (this.member.masked) border += 2
    if (this.member.copyable) border += 2
    if (this.member.qr) border += 2

    return border
  }

  show(template: TemplateRef<any>) {
    const masked = this.masked

    this.masked = this.member.masked
    this.dialogs
      .open(template, { label: 'Scan this QR', size: 's' })
      .subscribe({
        complete: () => (this.masked = masked),
      })
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
    el.type = this.masked && this.member.masked ? 'password' : 'text'
  }
}
