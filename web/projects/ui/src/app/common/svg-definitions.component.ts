import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  standalone: true,
  selector: 'svg-definitions',
  template: `
    <svg xmlns="http://www.w3.org/2000/svg">
      <defs>
        <filter id="bevel-light">
          <feFlood flood-color="white" flood-opacity="0.1" />
          <feComposite in2="SourceAlpha" operator="out" />
          <feGaussianBlur stdDeviation="0.5" result="blur" />
          <feOffset dx="-1" dy="1" />
          <feComposite operator="atop" in2="SourceGraphic" />
        </filter>
        <filter id="bevel-dark">
          <feFlood flood-color="black" flood-opacity="0.3" />
          <feComposite in2="SourceAlpha" operator="out" />
          <feGaussianBlur stdDeviation="0.5" result="blur" />
          <feOffset dx="1" dy="-1" />
          <feComposite operator="atop" in2="SourceGraphic" />
        </filter>
        <filter id="round-corners">
          <feGaussianBlur in="SourceGraphic" stdDeviation="3" result="blur" />
          <feColorMatrix
            in="blur"
            type="matrix"
            values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 19 -9"
            result="flt_tag"
          />
          <feComposite in="SourceGraphic" in2="flt_tag" operator="atop" />
        </filter>
      </defs>
    </svg>
  `,
  styles: `
    :host {
      position: absolute;
      width: 0;
      height: 0;
      visibility: hidden;
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class SvgDefinitionsComponent {}
