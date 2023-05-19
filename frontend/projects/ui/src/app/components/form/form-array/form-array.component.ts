import { Component, HostBinding, inject, Input } from '@angular/core'
import { AbstractControl, FormArrayName } from '@angular/forms'
import { TUI_PARENT_STOP, TuiDestroyService } from '@taiga-ui/cdk'
import {
  TUI_ANIMATION_OPTIONS,
  TuiDialogService,
  tuiFadeIn,
  tuiHeightCollapse,
} from '@taiga-ui/core'
import { TUI_PROMPT } from '@taiga-ui/kit'
import { filter, takeUntil } from 'rxjs'
import { ValueSpecList } from '@start9labs/start-sdk/lib/config/configTypes'
import { FormService } from '../../../services/form.service'
import { ERRORS } from '../form-group/form-group.component'

@Component({
  selector: 'form-array',
  templateUrl: './form-array.component.html',
  styleUrls: ['./form-array.component.scss'],
  animations: [tuiFadeIn, tuiHeightCollapse, TUI_PARENT_STOP],
  providers: [TuiDestroyService],
})
export class FormArrayComponent {
  @Input()
  spec!: ValueSpecList

  @HostBinding('@tuiParentStop')
  readonly animation = { value: '', ...inject(TUI_ANIMATION_OPTIONS) }
  readonly order = ERRORS
  readonly array = inject(FormArrayName)
  readonly open = new Map<AbstractControl, boolean>()

  private warned = false
  private readonly formService = inject(FormService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly destroy$ = inject(TuiDestroyService)

  get hint(): string {
    const hint = []

    if (this.spec.description) {
      hint.push(this.spec.description)
    }

    if (this.spec.disabled) {
      hint.push(`Disabled: ${this.spec.disabled}`)
    }

    return hint.join('\n\n')
  }

  get canAdd(): boolean {
    return (
      !this.spec.disabled &&
      (!this.spec.maxLength ||
        this.spec.maxLength >= this.array.control.controls.length)
    )
  }

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
        this.removeItem(index)
      })
  }

  private removeItem(index: number) {
    this.open.delete(this.array.control.at(index))
    this.array.control.removeAt(index)
  }

  private addItem() {
    this.array.control.insert(0, this.formService.getListItem(this.spec))
    this.open.set(this.array.control.at(0), true)
  }
}
