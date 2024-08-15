import {
  Component,
  DestroyRef,
  HostBinding,
  inject,
  Input,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import { AbstractControl, FormArrayName } from '@angular/forms'
import { CT } from '@start9labs/start-sdk'
import {
  TUI_ANIMATIONS_SPEED,
  TuiDialogService,
  tuiFadeIn,
  tuiHeightCollapse,
  tuiParentStop,
  tuiToAnimationOptions,
} from '@taiga-ui/core'
import { TUI_CONFIRM } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { FormService } from 'src/app/services/form.service'
import { ERRORS } from '../form-group/form-group.component'

@Component({
  selector: 'form-array',
  templateUrl: './form-array.component.html',
  styleUrls: ['./form-array.component.scss'],
  animations: [tuiFadeIn, tuiHeightCollapse, tuiParentStop],
})
export class FormArrayComponent {
  @Input({ required: true })
  spec!: CT.ValueSpecList

  @HostBinding('@tuiParentStop')
  readonly animation = tuiToAnimationOptions(inject(TUI_ANIMATIONS_SPEED))
  readonly order = ERRORS
  readonly array = inject(FormArrayName)
  readonly open = new Map<AbstractControl, boolean>()

  private warned = false
  private readonly formService = inject(FormService)
  private readonly dialogs = inject(TuiDialogService)
  private readonly destroyRef = inject(DestroyRef)

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
        .open<boolean>(TUI_CONFIRM, {
          label: 'Warning',
          size: 's',
          data: { content: this.spec.warning, yes: 'Ok', no: 'Cancel' },
        })
        .pipe(filter(Boolean), takeUntilDestroyed(this.destroyRef))
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
      .open<boolean>(TUI_CONFIRM, {
        label: 'Confirm',
        size: 's',
        data: {
          content: 'Are you sure you want to delete this entry?',
          yes: 'Delete',
          no: 'Cancel',
        },
      })
      .pipe(filter(Boolean), takeUntilDestroyed(this.destroyRef))
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
