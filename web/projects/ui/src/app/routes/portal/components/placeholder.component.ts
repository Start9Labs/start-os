import { ChangeDetectionStrategy, Component, input } from '@angular/core'
import { TuiIcon } from '@taiga-ui/core'

@Component({
  selector: 'app-placeholder',
  template: '<tui-icon [icon]="icon()" /><ng-content/>',
  styles: `
    :host {
      display: flex;
      flex: 1;
      flex-direction: column;
      gap: 0.5rem;
      align-items: center;
      justify-content: center;
      text-align: center;
      padding: 1rem;
      font: var(--tui-font-text-l);
      color: var(--tui-text-tertiary);

      tui-icon {
        font-size: 2.5rem;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [TuiIcon],
})
export class PlaceholderComponent {
  readonly icon = input.required<string>()
}
