import {
  ChangeDetectionStrategy,
  Component,
  EventEmitter,
  forwardRef,
  inject,
  Input,
  Output,
} from '@angular/core'
import { ControlContainer } from '@angular/forms'
import { IST } from '@start9labs/start-sdk'
import { TuiButton, TuiIcon } from '@taiga-ui/core'
import { TuiExpand } from '@taiga-ui/experimental'
import { TuiTooltip } from '@taiga-ui/kit'

import { ControlDirective } from './control.directive'
import { FormGroupComponent } from './group.component'

@Component({
  selector: 'form-object',
  template: `
    <h3 class="title" (click)="toggle()">
      <button
        tuiIconButton
        size="s"
        iconStart="@tui.chevron-down"
        type="button"
        class="button"
        [class.button_open]="open"
        [style.border-radius.%]="100"
        [appearance]="invalid ? 'primary-destructive' : 'secondary'"
      ></button>
      <ng-content />
      {{ spec.name }}
      @if (spec.description) {
        <tui-icon [tuiTooltip]="spec.description" (click.stop)="(0)" />
      }
    </h3>
    <tui-expand class="expand" [expanded]="open">
      <div class="g-form-group" [class.g-form-group_invalid]="invalid">
        <form-group [spec]="spec.spec" />
      </div>
    </tui-expand>
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    :host {
      display: flex;
      flex-direction: column;
      align-items: flex-start;
    }

    .title {
      position: relative;
      height: var(--tui-height-l);
      display: flex;
      align-items: center;
      cursor: pointer;
      font: var(--tui-font-text-l);
      font-weight: bold;
      margin: 0 0 -0.75rem;
    }

    .button {
      @include taiga.transition(transform);

      margin-right: 1rem;

      &_open {
        transform: rotate(180deg);
      }
    }

    .expand {
      align-self: stretch;
    }

    .g-form-group {
      padding-top: 0.75rem;

      &_invalid::before,
      &_invalid::after {
        background: var(--tui-status-negative-pale);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  hostDirectives: [ControlDirective],
  imports: [
    TuiButton,
    TuiIcon,
    TuiTooltip,
    TuiExpand,
    forwardRef(() => FormGroupComponent),
  ],
})
export class FormObjectComponent {
  @Input({ required: true })
  spec!: IST.ValueSpecObject

  @Input()
  open = false

  @Output()
  readonly openChange = new EventEmitter<boolean>()

  private readonly container = inject(ControlContainer)

  get invalid() {
    return !this.container.valid && this.container.touched
  }

  toggle() {
    this.open = !this.open
    this.openChange.emit(this.open)
  }
}
