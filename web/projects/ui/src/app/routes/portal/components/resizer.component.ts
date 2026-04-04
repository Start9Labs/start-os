import { ChangeDetectionStrategy, Component } from '@angular/core'

@Component({
  selector: 'input[type="range"][appResizer]',
  template: '',
  styles: `
    @use '@taiga-ui/styles/utils' as taiga;

    :host {
      @include taiga.transition(color);

      position: fixed;
      inset: 0 calc(320px - var(--bumper)) 0 calc(320px - 2 * var(--bumper));
      appearance: none;
      pointer-events: none;
      background: none;
      color: transparent;
      outline: none;
      cursor: ew-resize;

      &:hover {
        color: var(--tui-background-neutral-1-hover);
      }

      &::-webkit-slider-runnable-track {
        block-size: 100%;
      }

      &::-webkit-slider-thumb {
        block-size: 100%;
        inline-size: calc(var(--bumper) * 3);
        padding: var(--bumper);
        appearance: none;
        background: currentColor;
        background-clip: content-box;
        border: none;
        pointer-events: auto;
        border-radius: var(--tui-radius-l);
      }

      &::-moz-range-thumb {
        block-size: 100%;
        inline-size: calc(var(--bumper) * 3);
        padding: var(--bumper);
        appearance: none;
        background: currentColor;
        background-clip: content-box;
        border: none;
        pointer-events: auto;
        border-radius: var(--tui-radius-l);
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ResizerComponent {}
