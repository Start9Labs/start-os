import { NgTemplateOutlet } from '@angular/common'
import {
  Component,
  Directive,
  inject,
  Injector,
  OnDestroy,
  signal,
  TemplateRef,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiPopup, TuiPopupService } from '@taiga-ui/core'
import { TuiBlock, TuiSwitch } from '@taiga-ui/kit'
import {
  injectContext,
  PolymorpheusComponent,
  provideContext,
} from '@taiga-ui/polymorpheus'

@Component({
  template: `
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="open" />
      Help
    </label>
    <section *tuiPopup="open()" tuiAnimated class="modal-help">
      <ng-container *ngTemplateOutlet="template" />
    </section>
  `,
  styles: `
    :host {
      position: fixed;
      top: 0.75rem;
      right: 3.5rem;
      backdrop-filter: blur(1rem);
    }

    :host,
    [tuiBlock] {
      border-radius: 10rem;
    }

    ::ng-deep {
      .modal-help {
        position: fixed;
        inset: 4.5rem 1rem 1rem auto;
        width: 18rem;
        border-radius: var(--tui-radius-l);
        background: var(--tui-background-elevation-1);
        box-shadow: var(--tui-shadow-popup);
        backdrop-filter: blur(1rem);

        --tui-from: translate3d(150%, 0, 0);

        &.tui-enter,
        &.tui-leave {
          animation-name: tuiSlide;
        }
      }

      tui-root._mobile .modal-help {
        width: calc(100% - 2rem);
      }

      [tuiTheme='dark'] .modal-help {
        background:
          linear-gradient(45deg, rgba(82, 64, 168, 0.6117647059), transparent),
          linear-gradient(
            to bottom,
            rgba(82, 64, 168, 0.3294117647),
            transparent
          ),
          color-mix(
            in hsl,
            var(--tui-background-elevation-1) 90%,
            transparent 10%
          );
        background-blend-mode: multiply;
      }
    }
  `,
  imports: [
    FormsModule,
    NgTemplateOutlet,
    TuiBlock,
    TuiSwitch,
    TuiPopup,
    TuiAnimated,
  ],
})
class ModalToggle {
  readonly open = signal(false)
  readonly template = injectContext<TemplateRef<any>>()
}

@Directive({
  selector: 'ng-template[modalHelp]',
})
export class ModalHelp implements OnDestroy {
  readonly ref = inject(TuiPopupService).add(
    new PolymorpheusComponent(
      ModalToggle,
      Injector.create({
        providers: [provideContext(inject(TemplateRef))],
      }),
    ),
  )

  ngOnDestroy() {
    this.ref.destroy()
  }
}
