import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  standalone: true,
  selector: 'app-metric',
  template: `
    <header>{{ label }}</header>
    <ng-content />
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  styles: `
    :host {
      display: flex;
      flex-direction: column;
      flex: 1;
      border: 1px solid var(--tui-clear);
      border-radius: 0 1rem 1rem 1rem;
      box-shadow: 0 0 0.5rem rgba(0, 0, 0, 0.25);
      overflow: hidden;
    }

    header {
      position: relative;
      width: fit-content;
      font-size: 0.75rem;
      padding: 0.25rem 0.5rem;
      background: var(--tui-clear);

      &::before {
        position: absolute;
        top: 0;
        left: 100%;
        content: '';
        border-left: 1rem solid var(--tui-clear);
        border-bottom: 1.75rem solid transparent;
      }
    }

    :host-context(tui-root._mobile) {
      min-height: 8rem;
      min-width: 100%;
      border-radius: 0;
    }
  `,
})
export class MetricComponent {
  @Input() label = ''
}
