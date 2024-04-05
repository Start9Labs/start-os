import { CommonModule } from '@angular/common'
import { ChangeDetectionStrategy, Component, inject } from '@angular/core'
import { CopyService } from '@start9labs/shared'
import { TuiDialogContext } from '@taiga-ui/core'
import { TuiButtonModule } from '@taiga-ui/experimental'
import { POLYMORPHEUS_CONTEXT } from '@tinkoff/ng-polymorpheus'
import { QrCodeModule } from 'ng-qrcode'
import { ActionResponse } from 'src/app/services/api/api.types'

@Component({
  template: `
    {{ context.data.message }}
    <ng-container *ngIf="context.data.value">
      <qr-code
        *ngIf="context.data.qr"
        size="240"
        [value]="context.data.value"
      ></qr-code>
      <p>
        {{ context.data.value }}
        <button
          *ngIf="context.data.copyable"
          tuiIconButton
          appearance="flat"
          iconLeft="tuiIconCopyLarge"
          (click)="copyService.copy(context.data.value)"
        >
          Copy
        </button>
      </p>
    </ng-container>
  `,
  styles: [
    `
      qr-code {
        margin: 1rem auto;
        display: flex;
        justify-content: center;
      }

      p {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 0.5rem;
      }
    `,
  ],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [CommonModule, QrCodeModule, TuiButtonModule],
})
export class ServiceActionSuccessComponent {
  readonly copyService = inject(CopyService)
  readonly context =
    inject<TuiDialogContext<void, ActionResponse>>(POLYMORPHEUS_CONTEXT)
}
