import { NgTemplateOutlet } from '@angular/common'
import {
  ChangeDetectionStrategy,
  Component,
  computed,
  contentChildren,
  inject,
  input,
  linkedSignal,
  model,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiContext } from '@taiga-ui/cdk'

// TODO: Remove after Taiga UI 5.0
@Component({
  selector: 'tui-timeline-item',
  template: `
    @if (draggable()) {
      <input
        #drag
        type="range"
        class="t-input"
        [max]="host.total() + value()[0] - value()[1]"
        [style.--drag.%]="host.d() * (value()[1] - value()[0])"
        [(ngModel)]="offset"
        (ngModelChange)="update(drag)"
        (click)="drag.focus()"
      />
    }
    @if (resizable()) {
      <input
        type="range"
        class="t-input"
        [style.inset-inline-start.%]="host.d() * min()"
        [style.inset-inline-end.%]="host.d() * (host.total() - value()[1])"
        [min]="min()"
        [max]="value()[1] - 1"
        [(ngModel)]="value()[0]"
        (ngModelChange)="value.set([value()[0], value()[1]])"
      />
      <input
        type="range"
        class="t-input"
        [style.inset-inline-start.%]="host.d() * value()[0]"
        [style.inset-inline-end.%]="host.d() * (host.total() - max())"
        [min]="value()[0] + 1"
        [max]="max()"
        [(ngModel)]="value()[1]"
        (ngModelChange)="value.set([value()[0], value()[1]])"
      />
    }
    <ng-content />
  `,
  styles: `
    :host {
      position: absolute;
      inset: 0;
      pointer-events: none;
    }

    .t-input {
      position: fixed;
      inset: 0;
      margin: 0;
      appearance: none;
      pointer-events: none;
      background: none;
      outline: none;
      cursor: ew-resize;

      --drag: 1.5rem;

      :host-context(tui-timeline[data-orientation='vertical']) & {
        cursor: ns-resize;
      }

      &::-webkit-slider-runnable-track {
        block-size: 100%;
      }

      &::-webkit-slider-thumb {
        block-size: 100%;
        inline-size: var(--drag);
        appearance: none;
        pointer-events: auto;
        border-radius: var(--tui-radius-xs);
      }

      &::-moz-range-thumb {
        block-size: 100%;
        inline-size: var(--drag);
        opacity: 0;
        pointer-events: auto;
      }

      &:focus-visible::-webkit-slider-thumb {
        outline: 0.125rem solid var(--tui-border-focus);
      }
    }
  `,
  host: {
    '[style.inset-inline-start.%]': 'host.d() * offset()',
    '[style.inline-size.%]': 'host.d() * (value()[1] - value()[0])',
  },
  imports: [FormsModule],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TuiTimelineItem {
  protected readonly host = inject(TuiTimelineComponent)
  protected readonly offset = linkedSignal(() => this.value()[0])
  protected readonly min = computed((array = this.host.value()) =>
    array.reduce(
      (min, [_, end]) => (end <= this.value()[0] && end > min ? end : min),
      0,
    ),
  )

  protected readonly max = computed((array = this.host.value()) =>
    array.reduce(
      (max, [start]) => (start >= this.value()[1] && start < max ? start : max),
      this.host.total(),
    ),
  )

  readonly draggable = input(true)
  readonly resizable = input(true)
  readonly value = model<readonly [number, number]>([0, 0])

  protected update(input: HTMLInputElement): void {
    const offset = Math.max(
      Math.min(this.offset(), this.max() - this.value()[1] + this.value()[0]),
      this.min(),
    )

    input.valueAsNumber = offset
    this.offset.set(offset)
    this.value.update(([start, end]) => [offset, offset + end - start])
  }
}

@Component({
  selector: 'tui-timeline',
  template: `
    @for (_ of '-'.repeat(value().length + 1); track value()[$index]) {
      @let gap = gaps()[$index] ?? [0, 0];
      @if (gap[0] !== gap[1]) {
        <span
          [style.position]="'absolute'"
          [style.block-size.%]="100"
          [style.inline-size.%]="d() * (gap[1] - gap[0])"
          [style.inset-inline-start.%]="d() * gap[0]"
        >
          <ng-container
            [ngTemplateOutlet]="template()"
            [ngTemplateOutletContext]="{ $implicit: $index }"
          />
        </span>
      }
    }
    <ng-content />
  `,
  styles: `
    :host {
      display: block;
      transform: translate3d(0, 0, 0);

      &[data-orientation='vertical'] {
        writing-mode: vertical-lr;
      }
    }
  `,
  host: { '[attr.data-orientation]': 'orientation()' },
  imports: [NgTemplateOutlet],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class TuiTimelineComponent {
  protected readonly items = contentChildren(TuiTimelineItem)
  protected readonly gaps = computed(() =>
    [[0, this.total()], ...[...this.value()].sort(([a], [b]) => a - b)].map(
      ([_, end], i, a) => [i ? end : 0, a[i + 1]?.[0] ?? this.total()],
    ),
  )

  readonly orientation = input('horizontal')
  readonly template = input<TemplateRef<TuiContext<number>>>()
  readonly total = input(100)
  readonly d = computed(() => 100 / this.total())
  readonly value = computed(() => this.items().map(({ value }) => value()))
}

export const TuiTimeline = [TuiTimelineComponent, TuiTimelineItem] as const
