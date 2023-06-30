import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { getPkgId, CopyService } from '@start9labs/shared'
import { AddressInfo, DataModel } from 'src/app/services/patch-db/data-model'
import { PatchDB } from 'patch-db-client'
import { map } from 'rxjs'
import { QRComponent } from './qr.component'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'

@Component({
  selector: 'app-interfaces',
  templateUrl: './app-interfaces.page.html',
  styleUrls: ['./app-interfaces.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppInterfacesPage {
  readonly pkgId = getPkgId(this.route)
  readonly addressInfo$ = this.patch
    .watch$('package-data', this.pkgId, 'installed', 'address-info')
    .pipe(
      map(addressInfo =>
        Object.values(addressInfo).sort((a, b) => a.name.localeCompare(b.name)),
      ),
    )

  constructor(
    private readonly route: ActivatedRoute,
    private readonly patch: PatchDB<DataModel>,
  ) {}
}

@Component({
  selector: 'app-interfaces-item',
  templateUrl: './app-interfaces-item.component.html',
  styleUrls: ['./app-interfaces.page.scss'],
})
export class AppInterfacesItemComponent {
  @Input()
  addressInfo!: AddressInfo

  constructor(
    private readonly dialogs: TuiDialogService,
    readonly copyService: CopyService,
  ) {}

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  showQR(data: string) {
    this.dialogs
      .open(new PolymorpheusComponent(QRComponent), {
        size: 'auto',
        data,
      })
      .subscribe()
  }
}
