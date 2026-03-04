import {
  ChangeDetectionStrategy,
  Component,
  computed,
  input,
} from '@angular/core'
import { TuiTitle } from '@taiga-ui/core'
import { TuiCell } from '@taiga-ui/layout'
import { T } from '@start9labs/start-sdk'
import { ValuePipe } from './value.pipe'
import { i18nKey, i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'metrics-data',
  template: `
    @for (key of keys(); track $index) {
      <div tuiCell="m">
        <span tuiTitle>{{ labels()[key] | i18n }}</span>
        <span tuiTitle [attr.data-unit]="$any(value()?.[key])?.unit">
          {{ $any(value()?.[key])?.value | value }}
        </span>
      </div>
    }
  `,
  styles: `
    [tuiTitle] {
      &:first-child {
        color: var(--tui-text-tertiary);
      }

      &:last-child {
        flex-direction: row;
        gap: 0;
      }

      &:after {
        content: attr(data-unit);
        font: var(--tui-font-text-ui-xs);
        color: var(--tui-text-secondary);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiCell, TuiTitle, ValuePipe, i18nPipe],
})
export class DataComponent<M extends T.Metrics[keyof T.Metrics]> {
  readonly labels = input.required<Record<keyof M, i18nKey>>()
  readonly value = input<M>()
  readonly keys = computed(() => Object.keys(this.labels()) as Array<keyof M>)
}
