import {
  ChangeDetectionStrategy,
  Component,
  inject,
  Input,
} from '@angular/core'
import { RouterLink } from '@angular/router'
import { ErrorService, LoadingService } from '@start9labs/shared'
import { TuiButton, TuiLink } from '@taiga-ui/core'
import { TuiBadge } from '@taiga-ui/kit'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ConfigService } from 'src/app/services/config.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'
import { getManifest } from '../../../../../utils/get-package-data'
import { MappedInterface } from '../types/mapped-interface'

@Component({
  selector: 'tr[serviceInterface]',
  template: `
    <td>
      <a tuiLink [routerLink]="info.routerLink">
        <strong>{{ info.name }}</strong>
      </a>
    </td>
    <td>
      <tui-badge size="m" [appearance]="appearance">{{ info.type }}</tui-badge>
    </td>
    <td class="g-secondary" [style.grid-area]="'2 / span 4'">
      {{ info.description }}
    </td>
    <td>
      @if (info.public) {
        <button
          tuiButton
          size="s"
          iconStart="@tui.globe"
          appearance="positive"
          (click)="toggle()"
        >
          Public
        </button>
      } @else {
        <button
          tuiButton
          size="s"
          iconStart="@tui.lock"
          appearance="negative"
          (click)="toggle()"
        >
          Private
        </button>
      }
    </td>
    <td [style.grid-area]="'span 2'">
      @if (info.type === 'ui') {
        <a
          tuiIconButton
          appearance="action"
          iconStart="@tui.external-link"
          target="_blank"
          rel="noreferrer"
          size="s"
          [style.border-radius.%]="100"
          [attr.href]="href"
          (click.stop)="(0)"
        >
          Open
        </a>
      }
    </td>
  `,
  styles: `
    strong {
      white-space: nowrap;
    }

    tui-badge {
      text-transform: uppercase;
    }

    :host-context(tui-root._mobile) {
      display: grid;
      grid-template-columns: repeat(3, min-content) 1fr 2rem;
      align-items: center;
      padding: 1rem 0.5rem;
      gap: 0.5rem;

      td {
        padding: 0;
      }
    }
  `,
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [TuiButton, TuiBadge, TuiLink, RouterLink],
})
export class ServiceInterfaceComponent {
  private readonly config = inject(ConfigService)
  private readonly errorService = inject(ErrorService)
  private readonly loader = inject(LoadingService)
  private readonly api = inject(ApiService)

  @Input({ required: true })
  info!: MappedInterface

  @Input({ required: true })
  pkg!: PackageDataEntry

  @Input()
  disabled = false

  get appearance(): string {
    switch (this.info.type) {
      case 'ui':
        return 'primary'
      case 'api':
        return 'accent'
      case 'p2p':
        return 'primary-grayscale'
    }
  }

  get href(): string | null {
    return this.disabled
      ? 'null'
      : this.config.launchableAddress(this.info, this.pkg.hosts)
  }

  async toggle() {
    const loader = this.loader
      .open(`Making ${this.info.public ? 'private' : 'public'}`)
      .subscribe()

    const params = {
      internalPort: this.info.addressInfo.internalPort,
      public: !this.info.public,
    }

    try {
      if (!this.info.public) {
        await this.api.pkgBindingSetPubic({
          ...params,
          host: this.info.addressInfo.hostId,
          package: getManifest(this.pkg).id,
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
