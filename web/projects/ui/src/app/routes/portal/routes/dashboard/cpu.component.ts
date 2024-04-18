import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  standalone: true,
  selector: 'app-cpu',
  template: `
    <div class="meter"></div>
    <div class="arrow" [style.transform]="transform"></div>
    <div class="percent">{{ percent }}%</div>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      position: relative;
      margin: 1rem auto;
      width: 80%;
      aspect-ratio: 1;
      background: var(--tui-clear);
      border-radius: 100%;
    }

    .meter {
      position: absolute;
      inset: 7%;
      mask: url(/assets/img/meter.svg);
      background: conic-gradient(
        from 180deg,
        var(--tui-success-fill) 30%,
        var(--tui-warning-fill),
        var(--tui-error-fill) 70%
      );
    }

    .percent {
      position: absolute;
      bottom: 10%;
      width: 100%;
      text-align: center;
    }

    .arrow {
      @include transition(transform);
      position: absolute;
      top: 50%;
      left: 50%;

      &:before {
        content: '';
        position: absolute;
        inset: -0.5rem;
        background: var(--tui-base-02);
        border-radius: 100%;
      }

      &:after {
        content: '';
        position: absolute;
        width: 0.25rem;
        height: 2rem;
        border-radius: 1rem;
        background: var(--tui-text-01);
        transform: translate(-0.125rem, -0.125rem);
        clip-path: polygon(0 0, 100% 0, 60% 100%, 40% 100%);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class CpuComponent {
  @Input()
  value = 0

  get percent(): string {
    return (100 * this.value).toFixed(1)
  }

  get transform(): string {
    return `rotate(${60 + 300 * this.value}deg)`
  }
}
