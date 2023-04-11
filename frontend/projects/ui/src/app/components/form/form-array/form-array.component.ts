import {
  ChangeDetectionStrategy,
  Component,
  HostBinding,
  inject,
  Input,
} from '@angular/core'
import { FormArrayName } from '@angular/forms'
import { TuiDestroyService } from '@taiga-ui/cdk'
import {
  TUI_ANIMATION_OPTIONS,
  TuiDialogService,
  tuiFadeIn,
  tuiHeightCollapse,
} from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter, takeUntil } from 'rxjs'
import { ValueSpecList } from 'start-sdk/lib/config/configTypes'
import { FormService } from '../../../services/form.service'
import { ERRORS } from '../form-group/form-group.component'
import { transition, trigger } from '@angular/animations'

@Component({
  selector: 'form-array',
  templateUrl: './form-array.component.html',
  styleUrls: ['./form-array.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
  animations: [
    tuiFadeIn,
    tuiHeightCollapse,
    trigger('parent', [transition(':enter', [])]),
  ],
  providers: [TuiDestroyService],
})
export class FormArrayComponent {
  @Input()
  spec!: ValueSpecList

  warned = false

  @HostBinding('@parent')
  readonly animation = { value: '', ...inject(TUI_ANIMATION_OPTIONS) }
  readonly order = ERRORS
  readonly array = inject(FormArrayName)
  private readonly formService = inject(FormService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly destroy$ = inject(TuiDestroyService)

  add() {
    if (!this.warned && this.spec.warning) {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: { content: this.spec.warning, yes: 'Ok', no: 'Cancel' },
        })
        .pipe(filter(Boolean), takeUntil(this.destroy$))
        .subscribe(() => {
          this.addItem()
        })
    } else {
      this.addItem()
    }

    this.warned = true
  }

  removeAt(index: number) {
    this.dialogs
      .open<boolean>(TUI_PROMPT, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Are you sure you want to delete this entry?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean), takeUntil(this.destroy$))
      .subscribe(() => {
        this.array.control.removeAt(index)
      })
  }

  private addItem() {
    this.array.control.insert(0, this.formService.getListItem(this.spec))
  }
}
