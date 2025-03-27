import { ChangeDetectionStrategy, Component, input } from '@angular/core'

@Component({
  standalone: true,
  selector: 'table[appTable]',
  template: `
    <thead>
      <tr>
        @for (header of appTable(); track $index) {
          <th>{{ header }}</th>
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
})
export class TableComponent {
  readonly appTable = input.required<readonly string[]>()
}
