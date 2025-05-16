import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { getPkgId, i18nPipe } from '@start9labs/shared'
import { T } from '@start9labs/start-sdk'
import { TuiItem } from '@taiga-ui/cdk'
import { TuiButton, TuiLink } from '@taiga-ui/core'
import { TuiBadge, TuiBreadcrumbs } from '@taiga-ui/kit'
import { PatchDB } from 'patch-db-client'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { getAddresses } from 'src/app/routes/portal/components/interfaces/interface.utils'
import { InterfaceStatusComponent } from 'src/app/routes/portal/components/interfaces/status.component'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'

@Component({
  template: `
    <ng-container *title>
      <a routerLink="../.." tuiIconButton iconStart="@tui.arrow-left">
        {{ 'Back' | i18n }}
      </a>
      {{ interface()?.name }}
      <interface-status
        [style.margin-left.rem]="0.5"
        [public]="!!interface()?.public"
      />
    </ng-container>
    <tui-breadcrumbs size="l">
      <a *tuiItem tuiLink appearance="action-grayscale" routerLink="../..">
        {{ 'Dashboard' | i18n }}
      </a>
      <span *tuiItem class="name">
        {{ interface()?.name }}
        <tui-badge [appearance]="getAppearance(interface()?.type)">
          {{ interface()?.type }}
        </tui-badge>
        <interface-status [public]="!!interface()?.public" />
      </span>
    </tui-breadcrumbs>
    @if (interface(); as value) {
      <app-interface [packageId]="pkgId" [interface]="value" />
    }
  `,
  styles: `
    :host-context(tui-root._mobile) tui-breadcrumbs {
      display: none;
    }

    .name {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      color: var(--tui-text-primary);

      tui-badge {
        text-transform: uppercase;
        font-weight: bold;
      }
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [
    InterfaceComponent,
    RouterLink,
    TuiButton,
    TitleDirective,
    TuiBreadcrumbs,
    TuiItem,
    TuiLink,
    InterfaceStatusComponent,
    i18nPipe,
    TuiBadge,
  ],
})
export default class ServiceInterfaceRoute {
  private readonly config = inject(ConfigService)

  readonly pkgId = getPkgId()
  readonly interfaceId = input('')

  readonly pkg = toSignal(
    inject<PatchDB<DataModel>>(PatchDB).watch$('packageData', this.pkgId),
  )

  readonly interface = computed(() => {
    const pkg = this.pkg()
    const id = this.interfaceId()

    if (!pkg || !id) {
      return
    }

    const { serviceInterfaces, hosts } = pkg
    const item = serviceInterfaces[this.interfaceId()]
    const key = item?.addressInfo.hostId || ''
    const host = hosts[key]
    const port = item?.addressInfo.internalPort

    if (!host || !item || !port) {
      return
    }

    return {
      ...item,
      addSsl: host?.bindings[port]?.options.addSsl,
      public: !!host?.bindings[port]?.net.public,
      addresses: getAddresses(item, host, this.config),
    }
  })

  getAppearance(type: T.ServiceInterfaceType = 'ui'): string {
    switch (type) {
      case 'ui':
        return 'positive'
      case 'api':
        return 'info'
      case 'p2p':
        return 'negative'
    }
  }
}
