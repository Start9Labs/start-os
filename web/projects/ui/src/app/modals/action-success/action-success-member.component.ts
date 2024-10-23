import { CommonModule } from '@angular/common'
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
import {
  TuiDialogService,
  TuiTextfieldComponent,
  TuiTextfieldControllerModule,
} from '@taiga-ui/core'
import { TuiButtonModule, TuiTitleModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import { QrCodeModule } from 'ng-qrcode'
import { T } from '@start9labs/start-sdk'

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
        tuiTextfield
        [style.border-inline-end-width.rem]="border"
        [type]="member.masked && masked ? 'password' : 'text'"
      />
    </tui-input>
    <label *ngIf="member.description" [style.padding-top.rem]="0.25" tuiTitle>
      <span tuiSubtitle [style.opacity]="0.8">{{ member.description }}</span>
    </label>
    <ng-template #actions>
      <button
        *ngIf="member.masked"
        tuiIconButton
        appearance="icon"
        size="s"
        type="button"
        tabindex="-1"
        [iconLeft]="masked ? 'tuiIconEye' : 'tuiIconEyeOff'"
        [style.pointer-events]="'auto'"
        (click)="masked = !masked"
      >
        Reveal/Hide
      </button>
      <button
        *ngIf="member.copyable"
        tuiIconButton
        appearance="icon"
        size="s"
        type="button"
        tabindex="-1"
        iconLeft="tuiIconCopy"
        [style.pointer-events]="'auto'"
        (click)="copy()"
      >
        Copy
      </button>
      <button
        *ngIf="member.qr"
        tuiIconButton
        appearance="icon"
        size="s"
        type="button"
        tabindex="-1"
        iconLeft="tuiIconGrid"
        [style.pointer-events]="'auto'"
        (click)="show(qr)"
      >
        Show QR
      </button>
    </ng-template>
    <ng-template #qr>
      <qr-code
        [value]="member.value"
        [style.filter]="member.masked && masked ? 'blur(0.5rem)' : null"
        size="350"
      ></qr-code>
      <button
        *ngIf="member.masked && masked"
        tuiIconButton
        class="reveal"
        iconLeft="tuiIconEye"
        [style.border-radius.%]="100"
        (click)="masked = false"
      >
        Reveal
      </button>
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
    TuiButtonModule,
    QrCodeModule,
    TuiTitleModule,
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
