import { ChangeDetectionStrategy, Component, Input } from '@angular/core'

@Component({
  standalone: true,
  selector: 'app-temperature',
  template: `
    <svg viewBox="0 0 43 95" fill="none" xmlns="http://www.w3.org/2000/svg">
      <path
        d="M31.7543 56.2935C31.5285 56.1505 31.3425 55.9529 31.2135 55.7188C31.0846 55.4848 31.0168 55.2219 31.0165 54.9547V12.2071C31.0165 9.68211 30.0134 7.26051 28.228 5.47505C26.4425 3.68959 24.0209 2.68652 21.4959 2.68652C18.9708 2.68652 16.5492 3.68959 14.7638 5.47505C12.9783 7.26051 11.9753 9.68211 11.9753 12.2071V54.9547C11.9748 55.2214 11.9072 55.4837 11.7786 55.7174C11.65 55.951 11.4645 56.1485 11.2394 56.2915C8.41934 58.1285 6.12747 60.6696 4.59028 63.6636C3.05309 66.6576 2.32379 70.001 2.47448 73.3632C2.70143 78.3319 4.86361 83.0146 8.49859 86.4098C12.1336 89.8049 16.9527 91.6429 21.9254 91.5307C26.8981 91.4185 31.6294 89.365 35.1076 85.8094C38.5857 82.2537 40.5345 77.4783 40.5371 72.5043C40.5385 69.2847 39.7359 66.1157 38.2022 63.2848C36.6684 60.4539 34.4521 58.0508 31.7543 56.2935Z"
        stroke="var(--tui-background-neutral-1)"
        stroke-width="4"
        stroke-miterlimit="10"
        stroke-linecap="round"
      />
      <path
        d="M21.5011 82.0256C26.7592 82.0256 31.0217 77.7631 31.0217 72.505C31.0217 67.2469 26.7592 62.9844 21.5011 62.9844C16.243 62.9844 11.9805 67.2469 11.9805 72.505C11.9805 77.7631 16.243 82.0256 21.5011 82.0256Z"
        fill="#3853E3"
      />
      <rect
        x="18"
        y="10"
        width="7"
        height="60"
        rx="3"
        fill="var(--tui-background-neutral-1)"
      />
      <rect
        x="18"
        y="10"
        width="7"
        height="60"
        rx="3"
        class="bar"
        fill="url(#temperature)"
      />
      <defs>
        <linearGradient
          id="temperature"
          x1="21"
          y1="13"
          x2="21"
          y2="66"
          gradientUnits="userSpaceOnUse"
        >
          <stop offset="0" stop-color="#EC2E34" />
          <stop offset="0.35" stop-color="#C48510" />
          <stop offset="0.65" stop-color="#00A030" />
          <stop offset="1" stop-color="#325CE3" />
        </linearGradient>
      </defs>
    </svg>
    <span>{{ value || '-' }} C°</span>
  `,
  styles: `
    @import '@taiga-ui/core/styles/taiga-ui-local';

    :host {
      height: 100%;
      display: flex;
      align-items: center;
      margin: auto;
    }

    svg {
      width: auto;
      height: 75%;
    }

    span {
      width: 4rem;
      flex-shrink: 0;
      white-space: nowrap;
      text-align: center;
    }

    .bar {
      @include transition(clip-path);
      clip-path: inset(var(--fill) 0 0 0);
    }
  `,
  host: {
    '[style.--fill.%]': '100 - value',
  },
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TemperatureComponent {
  @Input()
  value = 0
}
