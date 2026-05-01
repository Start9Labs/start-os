import {
  Component,
  Directive,
  inject,
  Injector,
  OnDestroy,
} from '@angular/core'
import { FormsModule } from '@angular/forms'
import { TuiAnimated } from '@taiga-ui/cdk'
import { TuiPopup, TuiPopupService, TuiScrollbar } from '@taiga-ui/core'
import { TuiBlock, TuiSwitch } from '@taiga-ui/kit'
import {
  injectContext,
  PolymorpheusComponent,
  provideContext,
} from '@taiga-ui/polymorpheus'
import { HELP, HELP_OPEN, HELP_URL } from 'src/app/help/help'

@Component({
  template: `
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="open" />
      Help
    </label>
    <tui-scrollbar *tuiPopup="open()" tuiAnimated class="modal-help">
      <div class="g-help" [innerHTML]="content"></div>
    </tui-scrollbar>
  `,
  styles: `
    :host {
      position: fixed;
      inset-block-start: 0.5rem;
      inset-inline-end: 3rem;
      backdrop-filter: blur(1rem);
    }

    :host,
    [tuiBlock] {
      border-radius: 10rem;
    }

    .modal-help {
      position: fixed;
      inset: 3rem 1rem 1rem auto;
      width: 18rem;
      border-radius: var(--tui-radius-l);
      background: var(--tui-background-elevation-1);
      box-shadow: var(--tui-shadow-popup);
      outline: 1px solid var(--tui-border-normal);
      outline-offset: -1px;

      --tui-from: translate3d(150%, 0, 0);

      &.tui-enter,
      &.tui-leave {
        animation-name: tuiSlide;
      }
    }

    ::ng-deep tui-root._mobile .modal-help {
      width: calc(100% - 2rem);
    }
  `,
  imports: [
    FormsModule,
    TuiBlock,
    TuiSwitch,
    TuiPopup,
    TuiAnimated,
    TuiScrollbar,
  ],
})
class ModalToggle {
  protected readonly open = inject(HELP_OPEN)
  protected readonly content = injectContext<string>()
}

@Directive({
  selector: 'ng-template[modalHelp]',
})
export class ModalHelp implements OnDestroy {
  readonly ref = inject(TuiPopupService).add(
    new PolymorpheusComponent(
      ModalToggle,
      Injector.create({
        providers: [provideContext(inject(HELP)[inject(HELP_URL)])],
      }),
    ),
  )

  ngOnDestroy() {
    this.ref.destroy()
  }
}
