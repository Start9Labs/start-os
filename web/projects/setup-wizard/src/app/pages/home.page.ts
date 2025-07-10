import { Component, inject, OnInit } from '@angular/core'
import { RouterModule } from '@angular/router'
import { ErrorService } from '@start9labs/shared'
import { TuiButton, TuiIcon, TuiTitle } from '@taiga-ui/core'
import { TuiCardLarge, TuiCell } from '@taiga-ui/layout'
import { RecoverComponent } from 'src/app/components/recover.component'
import { ApiService } from 'src/app/services/api.service'
import { StateService } from 'src/app/services/state.service'

@Component({
  template: `
    <img class="logo" src="assets/img/icon.png" alt="Start9" />
    @if (!loading) {
      <section tuiCardLarge="compact">
        <header [style.padding-top.rem]="1.25">
          @if (recover) {
            <button
              tuiIconButton
              appearance="flat-grayscale"
              class="back"
              iconStart="@tui.chevron-left"
              (click)="recover = false"
            >
              Back
            </button>
          }
          {{ recover ? 'Recover Options' : 'StartOS Setup' }}
        </header>
        <div class="pages">
          <div class="options" [class.options_recover]="recover">
            <button tuiCell [routerLink]="error || recover ? null : '/storage'">
              <tui-icon icon="@tui.plus" />
              <span tuiTitle>
                <span class="g-positive">Start Fresh</span>
                <span tuiSubtitle>
                  Get started with a brand new Start9 server
                </span>
              </span>
            </button>
            <button
              tuiCell
              [disabled]="error || recover"
              (click)="recover = true"
            >
              <tui-icon icon="@tui.rotate-cw" />
              <span tuiTitle>
                <span class="g-warning">Recover</span>
                <span tuiSubtitle>
                  Recover, restore, or transfer StartOS data
                </span>
              </span>
            </button>
          </div>
          <app-recover class="options" [disabled]="!recover" />
        </div>
      </section>
    }
  `,
  styles: `
    @use '@taiga-ui/core/styles/taiga-ui-local' as taiga;

    .logo {
      width: 6rem;
      margin: auto auto -2rem;
      z-index: 1;

      &:only-child {
        margin: auto;
      }

      + * {
        margin-top: 0;
      }
    }

    .back {
      position: absolute;
      top: 1rem;
      border-radius: 10rem;
    }

    .pages {
      display: flex;
      align-items: center;
      overflow: hidden;
    }

    .options {
      @include taiga.transition(margin);

      min-width: 100%;
      display: flex;
      flex-direction: column;
      gap: 1.25rem;
      padding: 1rem;
      box-sizing: border-box;

      &_recover {
        margin-left: -100%;
      }
    }
  `,
  imports: [
    RouterModule,
    TuiCardLarge,
    TuiButton,
    TuiCell,
    TuiIcon,
    TuiTitle,
    RecoverComponent,
  ],
})
export default class HomePage implements OnInit {
  private readonly api = inject(ApiService)
  private readonly errorService = inject(ErrorService)
  private readonly stateService = inject(StateService)

  error = false
  loading = true
  recover = false

  async ngOnInit() {
    this.stateService.setupType = 'fresh'

    try {
      await this.api.getPubKey()
    } catch (e: any) {
      this.error = true
      this.errorService.handleError(e)
    } finally {
      this.loading = false
    }
  }
}
