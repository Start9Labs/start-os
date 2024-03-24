import { CommonModule } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
  OnChanges,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { CopyService, ErrorService, LoadingService } from '@start9labs/shared'
import { TuiTextfieldControllerModule } from '@taiga-ui/core'
import { TuiButtonModule, TuiIconModule } from '@taiga-ui/experimental'
import { TuiInputModule, TuiInputNumberModule } from '@taiga-ui/kit'
import { PortForward } from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'tr[portForward]',
  template: `
    <td [style.text-align]="'right'">
      <tui-icon
        *ngIf="portForward.error; else noError"
        icon="tuiIconClose"
        [style.color]="'var(--tui-negative)'"
      />
      <ng-template #noError>
        <tui-icon icon="tuiIconCheck" [style.color]="'var(--tui-positive)'" />
      </ng-template>
    </td>
    <td>
      <tui-input-number
        decimal="never"
        [(ngModel)]="value"
        [readOnly]="!editing"
        [min]="0"
        [tuiTextfieldCustomContent]="buttons"
      >
        <input tuiTextfield type="text" [style.font-size.rem]="1" />
      </tui-input-number>
      <ng-template #buttons>
        <button
          *ngIf="!editing; else actions"
          tuiIconButton
          appearance="icon"
          iconLeft="tuiIconEdit2"
          size="s"
          (click)="toggle(true)"
        >
          Edit
        </button>
        <ng-template #actions>
          <button
            tuiIconButton
            appearance="icon"
            iconLeft="tuiIconClose"
            size="s"
            (click)="toggle(false)"
          >
            Cancel
          </button>
          <button
            tuiIconButton
            appearance="icon"
            iconLeft="tuiIconCheck"
            size="s"
            [disabled]="!value"
            (click)="save()"
          >
            Save
          </button>
        </ng-template>
      </ng-template>
    </td>
    <td>{{ ip }}:{{ portForward.target }}</td>
    <td>
      <button
        tuiIconButton
        appearance="icon"
        iconLeft="tuiIconCopy"
        size="s"
        (click)="copyService.copy(ip + ':' + portForward.target)"
      >
        Copy
      </button>
    </td>
  `,
  styles: ['button { pointer-events: auto }'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    CommonModule,
    FormsModule,
    TuiIconModule,
    TuiInputModule,
    TuiButtonModule,
    TuiInputNumberModule,
    TuiTextfieldControllerModule,
  ],
})
export class RouterPortComponent implements OnChanges {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)
  readonly copyService = inject(CopyService)

  @Input({ required: true })
  portForward!: PortForward

  @Input()
  ip = ''

  value = NaN
  editing = false

  ngOnChanges() {
    this.value = this.portForward.override || this.portForward.assigned
  }

  toggle(editing: boolean) {
    this.editing = editing
    this.value = this.portForward.override || this.portForward.assigned
  }

  async save() {
    const loader = this.loader.open('Saving...').subscribe()
    const { target } = this.portForward

    try {
      await this.api.overridePortForward({ target, port: this.value })
      this.portForward.override = this.value
      this.editing = false
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
