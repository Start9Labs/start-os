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
import { TuiButton } from '@taiga-ui/core'
import { PatchDB } from 'patch-db-client'
import { InterfaceComponent } from 'src/app/routes/portal/components/interfaces/interface.component'
import { ConfigService } from 'src/app/services/config.service'
import { DataModel } from 'src/app/services/patch-db/data-model'
import { TitleDirective } from 'src/app/services/title.service'
import { getAddresses } from '../../../components/interfaces/interface.utils'

@Component({
  template: `
    <ng-container *title>
      <a routerLink="../.." tuiIconButton iconStart="@tui.arrow-left">Back</a>
      {{ interface()?.name }}
    </ng-container>
    @if (interface(); as serviceInterface) {
      <app-interface
        [packageId]="pkgId"
        [serviceInterface]="serviceInterface"
      />
    }
  `,
  host: { class: 'g-subpage' },
  changeDetection: ChangeDetectionStrategy.OnPush,
  standalone: true,
  imports: [InterfaceComponent, RouterLink, TuiButton, TitleDirective],
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
    const host = hosts[item.addressInfo.hostId]

    return {
      ...item,
      public: host.bindings[item.addressInfo.internalPort].net.public,
      addresses: getAddresses(item, host, this.config),
    }
  })
}
