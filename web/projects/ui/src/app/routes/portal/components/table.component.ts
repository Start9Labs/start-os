import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { i18nKey, i18nPipe } from '@start9labs/shared'

@Component({
  selector: 'table[appTable]',
  template: `
    <thead>
      <tr>
        <ng-content select="th" />
        @for (header of appTable(); track $index) {
          <th>{{ header | i18n }}</th>
        }
      </tr>
    </thead>
    <tbody><ng-content /></tbody>
  `,
  styles: `
    :host:has(app-placeholder) thead {
      display: none;
    }
  `,
  host: { class: 'g-table' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [i18nPipe],
})
export class TableComponent {
  readonly appTable = input.required<Array<i18nKey | null>>()
}
