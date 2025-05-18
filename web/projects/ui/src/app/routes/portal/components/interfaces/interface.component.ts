import {
  ChangeDetectionStrategy,
  Component,
  inject,
  input,
} from '@angular/core'
import { ErrorService, i18nPipe, LoadingService } from '@start9labs/shared'
import { TuiButton, tuiButtonOptionsProvider } from '@taiga-ui/core'
import { InterfaceClearnetComponent } from 'src/app/routes/portal/components/interfaces/clearnet.component'
import { InterfaceLocalComponent } from 'src/app/routes/portal/components/interfaces/local.component'
import { InterfaceTorComponent } from 'src/app/routes/portal/components/interfaces/tor.component'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { MappedServiceInterface } from './interface.utils'

@Component({
  standalone: true,
  selector: 'app-interface',
  template: `
    <button
      tuiButton
      size="s"
      [appearance]="value().public ? 'primary-destructive' : 'primary-success'"
      [iconStart]="value().public ? '@tui.globe-lock' : '@tui.globe'"
      (click)="toggle()"
    >
      {{ value().public ? ('Make private' | i18n) : ('Make public' | i18n) }}
    </button>
    <section
      [clearnet]="value().addresses.clearnet"
      [isRunning]="isRunning()"
    ></section>
    <section [tor]="value().addresses.tor" [isRunning]="isRunning()"></section>
    <section
      [local]="value().addresses.local"
      [isRunning]="isRunning()"
    ></section>
  `,
  styles: `
    :host {
      max-width: 56rem;
      display: flex;
      flex-direction: column;
      gap: 1rem;
      color: var(--tui-text-secondary);
      font: var(--tui-font-text-l);
    }

    button {
      margin: -0.5rem auto 0 0;
    }
  `,
  providers: [tuiButtonOptionsProvider({ size: 'xs' })],
  changeDetection: ChangeDetectionStrategy.OnPush,
  imports: [
    InterfaceClearnetComponent,
    InterfaceTorComponent,
    InterfaceLocalComponent,
    TuiButton,
    i18nPipe,
  ],
})
export class InterfaceComponent {
  private readonly loader = inject(LoadingService)
  private readonly errorService = inject(ErrorService)
  private readonly api = inject(ApiService)

  readonly packageId = input('')
  readonly value = input.required<MappedServiceInterface>()
  readonly isRunning = input.required<boolean>()

  async toggle() {
    const loader = this.loader
      .open(`Making ${this.value().public ? 'private' : 'public'}`)
      .subscribe()

    const params = {
      internalPort: this.value().addressInfo.internalPort,
      public: !this.value().public,
    }

    try {
      if (this.packageId()) {
        await this.api.pkgBindingSetPubic({
          ...params,
          host: this.value().addressInfo.hostId,
          package: this.packageId(),
        })
      } else {
        await this.api.serverBindingSetPubic(params)
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
