import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  standalone: true,
  selector: 'app-metrics',
  template: `
    <ng-content />
    <section>TODO</section>
  `,
  styles: `
    :host {
      grid-column: 1/3;

      --clip-path: polygon(
        0 2rem,
        1.25rem 0,
        8.75rem 0,
        calc(10rem + 0.1em) calc(2rem - 0.1em),
        11rem 2rem,
        calc(65% - 0.2em) 2rem,
        calc(65% + 1.25rem) 0,
        calc(100% - 1.25rem) 0,
        100% 2rem,
        100% calc(100% - 2rem),
        calc(100% - 1.25rem) 100%,
        10.5rem 100%,
        calc(9.25rem - 0.1em) calc(100% - 2rem + 0.1em),
        1.25rem calc(100% - 2rem),
        0 calc(100% - 4rem)
      );

      section {
        height: 80%;
        display: flex;
        align-items: center;
        justify-content: center;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MetricsComponent {}
