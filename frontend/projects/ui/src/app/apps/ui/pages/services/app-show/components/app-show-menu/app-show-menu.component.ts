import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import {
  DataModel,
  PackageDataEntry,
} from 'src/app/services/patch-db/data-model'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { MarkdownComponent } from '@start9labs/shared'
import { from, map } from 'rxjs'
import { PatchDB } from 'patch-db-client'
import { NavController } from '@ionic/angular'
import { ActivatedRoute, Params } from '@angular/router'
import { FormDialogService } from 'src/app/services/form-dialog.service'
import { ProxyService } from 'src/app/services/proxy.service'
import {
  AppConfigPage,
  PackageConfigData,
} from '../../modals/app-config/app-config.page'

@Component({
  selector: 'app-show-menu',
  templateUrl: './app-show-menu.component.html',
  styleUrls: ['./app-show-menu.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowMenuComponent {
  @Input({ required: true })
  pkg!: PackageDataEntry

  get highlighted$() {
    return this.patch
      .watch$('ui', 'ack-instructions', this.pkg.manifest.id)
      .pipe(map(seen => !seen))
  }

  constructor(
    private readonly route: ActivatedRoute,
    private readonly navCtrl: NavController,
    private readonly dialogs: TuiDialogService,
    private readonly formDialog: FormDialogService,
    private readonly api: ApiService,
    readonly patch: PatchDB<DataModel>,
    private readonly proxyService: ProxyService,
  ) {}

  async presentModalInstructions() {
    const { id, version } = this.pkg.manifest

    this.api
      .setDbValue<boolean>(['ack-instructions', id], true)
      .catch(e => console.error('Failed to mark instructions as seen', e))

    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: 'Instructions',
        size: 'l',
        data: {
          content: from(
            this.api.getStatic(
              `/public/package-data/${id}/${version}/INSTRUCTIONS.md`,
            ),
          ),
        },
      })
      .subscribe()
  }

  openConfig() {
    this.formDialog.open<PackageConfigData>(AppConfigPage, {
      label: `${this.pkg.manifest.title} configuration`,
      data: { pkgId: this.pkg.manifest.id },
    })
  }

  setOutboundProxy() {
    this.proxyService.presentModalSetOutboundProxy({
      packageId: this.pkg.manifest.id,
      outboundProxy: this.pkg.installed!.outboundProxy,
      hasP2P: Object.values(this.pkg.installed!.interfaceInfo).some(
        i => i.type === 'p2p',
      ),
    })
  }

  navigate(path: string, qp?: Params) {
    return this.navCtrl.navigateForward([path], {
      relativeTo: this.route,
      queryParams: qp,
    })
  }
}
