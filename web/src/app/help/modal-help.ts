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
import { HELP, HELP_URL } from 'src/app/help/help'
import { SidebarService } from 'src/app/services/sidebar.service'

@Component({
  template: `
    <label tuiBlock="s" appearance="secondary-grayscale">
      <input type="checkbox" tuiSwitch size="s" [(ngModel)]="service.end" />
      Help
    </label>
    <tui-scrollbar *tuiPopup="service.end()" tuiAnimated class="modal-help">
      <div class="g-help" [innerHTML]="content"></div>
    </tui-scrollbar>
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

    ::ng-deep tui-root._mobile .modal-help {
      width: calc(100% - 2rem);
    }

    ::ng-deep [tuiTheme='dark'] .modal-help {
      background:
        linear-gradient(45deg, rgba(82, 64, 168, 0.6), transparent),
        linear-gradient(to bottom, rgba(82, 64, 168, 0.33), transparent),
        color-mix(
          in hsl,
          var(--tui-background-elevation-1) 90%,
          transparent 10%
        );
      background-blend-mode: multiply;
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
  protected readonly service = inject(SidebarService)
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
