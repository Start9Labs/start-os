import { AsyncPipe } from '@angular/common'
import {
  Component,
  DestroyRef,
  forwardRef,
  HostBinding,
  inject,
  Input,
} from '@angular/core'
import { takeUntilDestroyed } from '@angular/core/rxjs-interop'
import {
  AbstractControl,
  FormArrayName,
  ReactiveFormsModule,
} from '@angular/forms'
import { DialogService, i18nKey, i18nPipe } from '@start9labs/shared'
import { IST } from '@start9labs/start-sdk'
import {
  TUI_ANIMATIONS_SPEED,
  TuiButton,
  TuiError,
  tuiFadeIn,
  tuiHeightCollapse,
  TuiIcon,
  TuiLink,
  tuiParentStop,
  TuiTextfield,
  tuiToAnimationOptions,
} from '@taiga-ui/core'
import { TuiFieldErrorPipe, TuiTooltip } from '@taiga-ui/kit'
import { filter } from 'rxjs'
import { FormService } from 'src/app/services/form.service'

import { HintPipe } from '../pipes/hint.pipe'
import { MustachePipe } from '../pipes/mustache.pipe'
import { ERRORS, FormControlComponent } from './control.component'
import { ControlDirective } from './control.directive'
import { FormObjectComponent } from './object.component'

@Component({
  selector: 'form-array',
  template: `
    <div class="label">
      {{ spec.name }}
      @if (spec.description || spec.disabled) {
        <tui-icon [tuiTooltip]="spec | hint" />
      }
      <button
        tuiLink
        type="button"
        class="add"
        [disabled]="!canAdd"
        (click)="add()"
      >
        + {{ 'Add' | i18n }}
      </button>
    </div>
    <tui-error [error]="order | tuiFieldError | async" />
    @for (item of array.control.controls; track item) {
      @if (spec.spec.type === 'object') {
        <form-object
          class="object"
          [class.object_open]="!!open.get(item)"
          [formGroup]="$any(item)"
          [spec]="$any(spec.spec)"
          [@tuiHeightCollapse]="animation"
          [@tuiFadeIn]="animation"
          [open]="!!open.get(item)"
          (openChange)="open.set(item, $event)"
        >
          {{ item.value | mustache: $any(spec.spec).displayAs }}
          <button
            tuiIconButton
            type="button"
            class="remove"
            iconStart="@tui.trash"
            appearance="icon"
            size="m"
            title="Remove"
            (click.stop)="removeAt($index)"
          ></button>
        </form-object>
      } @else {
        <form-control
          class="control"
          tuiTextfieldSize="m"
          [formControl]="$any(item)"
          [spec]="$any(spec.spec)"
          [@tuiHeightCollapse]="animation"
          [@tuiFadeIn]="animation"
          (remove)="removeAt($index)"
        />
      }
    }
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      display: block;
      margin: 2rem 0;
    }

    .label {
      display: flex;
      font-size: 1.25rem;
      font-weight: bold;
    }

    .add {
      font-size: 1rem;
      padding: 0 1rem;
      margin-left: auto;
    }

    .object {
      display: block;
      position: relative;

      &_open::after,
      &:last-child::after {
        opacity: 0;
      }

      &::after {
        @include taiga.transition(opacity);

        content: '';
        position: absolute;
        bottom: -0.5rem;
        height: 1px;
        left: 3rem;
        right: 1rem;
        background: var(--tui-background-neutral-1);
      }
    }

    .remove {
      margin: 0 0.375rem 0 auto;
      pointer-events: auto;

      &::before {
        font-size: 1rem;
      }
    }

    .control {
      display: block;
      margin: 0.5rem 0;
    }
  `,
  animations: [tuiFadeIn, tuiHeightCollapse, tuiParentStop],
  hostDirectives: [ControlDirective],
  imports: [
    AsyncPipe,
    ReactiveFormsModule,
    TuiIcon,
    TuiTooltip,
    TuiLink,
    TuiError,
    TuiFieldErrorPipe,
    TuiButton,
    TuiTextfield,
    i18nPipe,
    HintPipe,
    MustachePipe,
    FormControlComponent,
    forwardRef(() => FormObjectComponent),
  ],
})
export class FormArrayComponent {
  @Input({ required: true })
  spec!: IST.ValueSpecList

  @HostBinding('@tuiParentStop')
  readonly animation = tuiToAnimationOptions(inject(TUI_ANIMATIONS_SPEED))
  readonly order = ERRORS
  readonly array = inject(FormArrayName)
  readonly open = new Map<AbstractControl, boolean>()

  private warned = false
  private readonly formService = inject(FormService)
  private readonly destroyRef = inject(DestroyRef)
  private readonly dialog = inject(DialogService)

  get canAdd(): boolean {
    return (
      !this.spec.disabled &&
      (!this.spec.maxLength ||
        this.spec.maxLength >= this.array.control.controls.length)
    )
  }

  add() {
    if (!this.warned && this.spec.warning) {
      this.dialog
        .openConfirm<boolean>({
          label: 'Warning',
          size: 's',
          data: {
            content: this.spec.warning as i18nKey,
            yes: 'Ok',
            no: 'Cancel',
          },
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
    this.removeItem(index)
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
