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
import { T } from '@start9labs/start-sdk'
import {
  TuiDialogService,
  TuiLabelModule,
  TuiTextfieldComponent,
  TuiTextfieldControllerModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import { QrCodeModule } from 'ng-qrcode'
import { ActionSuccessGroupComponent } from './action-success-group.component'

@Component({
  standalone: true,
  selector: 'app-action-success-item',
  template: `
    <p *ngIf="!parent" class="qr">
      <ng-container *ngTemplateOutlet="qr"></ng-container>
    </p>
    <label [tuiLabel]="value.description">
      <tui-input
        [readOnly]="true"
        [ngModel]="value.value"
        [tuiTextfieldCustomContent]="actions"
      >
        <input
          tuiTextfield
          [style.border-inline-end-width.rem]="border"
          [type]="value.masked && masked ? 'password' : 'text'"
        />
      </tui-input>
    </label>
    <ng-template #actions>
      <button
        *ngIf="value.masked"
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
        *ngIf="value.copyable"
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
        *ngIf="value.qr && parent"
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
        [value]="value.value"
        [style.filter]="value.masked && masked ? 'blur(0.5rem)' : null"
        size="350"
      ></qr-code>
      <button
        *ngIf="value.masked && masked"
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
    TuiLabelModule,
  ],
})
export class ActionSuccessItemComponent {
  @ViewChild(TuiTextfieldComponent, { read: ElementRef })
  private readonly input!: ElementRef<HTMLInputElement>
  private readonly dialogs = inject(TuiDialogService)

  readonly parent = inject(ActionSuccessGroupComponent, {
    optional: true,
  })

  @Input()
  value!: T.ActionResultV1 & { type: 'string' }

  masked = true

  get border(): number {
    let border = 0

    if (this.value.masked) {
      border += 2
    }

    if (this.value.copyable) {
      border += 2
    }

    if (this.value.qr && this.parent) {
      border += 2
    }

    return border
  }

  show(template: TemplateRef<any>) {
    const masked = this.masked

    this.masked = this.value.masked
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
    el.type = this.masked && this.value.masked ? 'password' : 'text'
  }
}
