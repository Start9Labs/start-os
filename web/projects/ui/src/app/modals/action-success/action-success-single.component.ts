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
  TuiLabelModule,
  TuiTextfieldComponent,
  TuiTextfieldControllerModule,
} from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { TuiInputModule } from '@taiga-ui/kit'
import { QrCodeModule } from 'ng-qrcode'
import { SingleResult } from './types'

@Component({
  standalone: true,
  selector: 'app-action-success-single',
  template: `
    <p class="qr">
      <ng-container *ngTemplateOutlet="qr"></ng-container>
    </p>
    <tui-input
      [readOnly]="true"
      [ngModel]="single.value"
      [tuiTextfieldLabelOutside]="true"
      [tuiTextfieldCustomContent]="actions"
    >
      <input
        tuiTextfield
        [style.border-inline-end-width.rem]="border"
        [type]="single.masked && masked ? 'password' : 'text'"
      />
    </tui-input>
    <ng-template #actions>
      <button
        *ngIf="single.masked"
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
        *ngIf="single.copyable"
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
    </ng-template>
    <ng-template #qr>
      <qr-code
        [value]="single.value"
        [style.filter]="single.masked && masked ? 'blur(0.5rem)' : null"
        size="350"
      ></qr-code>
      <button
        *ngIf="single.masked && masked"
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
export class ActionSuccessSingleComponent {
  @ViewChild(TuiTextfieldComponent, { read: ElementRef })
  private readonly input!: ElementRef<HTMLInputElement>
  private readonly dialogs = inject(TuiDialogService)

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
