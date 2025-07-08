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
import { TuiButton, TuiIcon, TuiNumberFormat } from '@taiga-ui/core'
import {
  TuiInputModule,
  TuiInputNumberModule,
  TuiTextfieldControllerModule,
} from '@taiga-ui/legacy'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PortForward } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'tr[portForward]',
  template: `
    <td [style.text-align]="'right'">
      <tui-icon
        *ngIf="portForward.error; else noError"
        icon="@tui.x"
        [style.color]="'var(--tui-text-negative)'"
      />
      <ng-template #noError>
        <tui-icon
          icon="@tui.check"
          [style.color]="'var(--tui-text-positive)'"
        />
      </ng-template>
    </td>
    <td>
      <tui-input-number
        [tuiNumberFormat]="{ precision: 0 }"
        [(ngModel)]="value"
        [readOnly]="!editing"
        [min]="0"
        [tuiTextfieldCustomContent]="buttons"
      >
        <input tuiTextfieldLegacy type="text" />
      </tui-input-number>
      <ng-template #buttons>
        <button
          *ngIf="!editing; else actions"
          tuiIconButton
          appearance="icon"
          iconStart="@tui.pencil"
          size="s"
          (click)="toggle(true)"
        >
          Edit
        </button>
        <ng-template #actions>
          <button
            tuiIconButton
            appearance="icon"
            iconStart="@tui.x"
            size="s"
            (click)="toggle(false)"
          >
            Cancel
          </button>
          <button
            tuiIconButton
            appearance="icon"
            iconStart="@tui.check"
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
        iconStart="@tui.copy"
        size="s"
        (click)="copyService.copy(ip + ':' + portForward.target)"
      >
        Copy
      </button>
    </td>
  `,
  styles: `
    button {
      pointer-events: auto;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    CommonModule,
    FormsModule,
    TuiIcon,
    TuiInputModule,
    TuiButton,
    TuiInputNumberModule,
    TuiTextfieldControllerModule,
    TuiNumberFormat,
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
    const loader = this.loader.open('Saving').subscribe()
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
