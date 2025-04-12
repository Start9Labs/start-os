import {
  ChangeDetectionStrategy,
  Component,
  computed,
  inject,
  input,
} from '@angular/core'
import { toSignal } from '@angular/core/rxjs-interop'
import { RouterLink } from '@angular/router'
import { getPkgId } from '@start9labs/shared'
import { TuiItem } from '@taiga-ui/cdk'
import { TuiButton, TuiLink } from '@taiga-ui/core'
import { TuiBreadcrumbs } from '@taiga-ui/kit'
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
      <a routerLink="../.." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      {{ interface()?.name }}
      <interface-status [public]="!!interface()?.public" />
    </ng-container>
    <tui-breadcrumbs size="l" [style.margin-block-end.rem]="1">
      <a *tuiItem tuiLink appearance="action-grayscale" routerLink="../..">
        Dashboard
      </a>
      <span *tuiItem class="g-primary">
        {{ interface()?.name }}
        <interface-status [public]="!!interface()?.public" />
      </span>
    </tui-breadcrumbs>
    @if (interface(); as serviceInterface) {
      <app-interface
        [packageId]="pkgId"
        [serviceInterface]="serviceInterface"
      />
    }
  `,
  styles: `
    :host-context(tui-root._mobile) tui-breadcrumbs {
      display: none;
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

    if (!host || !item) {
      return
    }

    return {
      ...item,
      public: !!host?.bindings[item.addressInfo.internalPort]?.net.public,
      addresses: getAddresses(item, host, this.config),
    }
  })
}
