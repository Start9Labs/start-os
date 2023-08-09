import {
  ChangeDetectionStrategy,
  Component,
  Inject,
  Input,
} from '@angular/core'
import {
  AbstractMarketplaceService,
  MarketplacePkg,
} from '@start9labs/marketplace'
import {
  Emver,
  ErrorService,
  isEmptyObject,
  LoadingService,
  sameUrl,
} from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { filter, firstValueFrom, of, Subscription, switchMap } from 'rxjs'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { ClientStorageService } from 'src/app/services/client-storage.service'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import { hasCurrentDeps } from 'src/app/util/has-deps'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { Breakages } from 'src/app/services/api/api.types'
import { PatchDB } from 'patch-db-client'
import { getAllPackages } from 'src/app/util/get-package-data'
import { TUI_PROMPT } from '@taiga-ui/kit'

@Component({
  selector: 'marketplace-show-controls',
  templateUrl: 'marketplace-show-controls.component.html',
  styleUrls: ['./marketplace-show-controls.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class MarketplaceShowControlsComponent {
  @Input()
  url?: string

  @Input({ required: true })
  pkg!: MarketplacePkg

  @Input()
  localPkg!: PackageDataEntry | null

  readonly showDevTools$ = this.ClientStorageService.showDevTools$

  readonly PackageState = PackageState

  constructor(
    private readonly dialogs: TuiDialogService,
    private readonly ClientStorageService: ClientStorageService,
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly loader: LoadingService,
    private readonly emver: Emver,
    private readonly errorService: ErrorService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  get localVersion(): string {
    return this.localPkg?.manifest.version || ''
  }

  async tryInstall() {
    const currentMarketplace = await firstValueFrom(
      this.marketplaceService.getSelectedHost$(),
    )
    const url = this.url || currentMarketplace.url

    if (!this.localPkg) {
      this.alertInstall(url)
    } else {
      const originalUrl = this.localPkg.installed?.['marketplace-url']

      if (!sameUrl(url, originalUrl)) {
        const proceed = await this.presentAlertDifferentMarketplace(
          url,
          originalUrl,
        )
        if (!proceed) return
      }

      const { id, version } = this.pkg.manifest

      const currentDeps = await hasCurrentDeps(this.patch, id)
      if (currentDeps && this.emver.compare(this.localVersion, version) !== 0) {
        this.dryInstall(url)
      } else {
        this.install(url)
      }
    }
  }

  private async presentAlertDifferentMarketplace(
    url: string,
    originalUrl: string | null | undefined,
  ): Promise<boolean> {
    const marketplaces = await firstValueFrom(
      this.patch.watch$('ui', 'marketplace'),
    )

    const name: string = marketplaces['known-hosts'][url]?.name || url

    let originalName: string | undefined
    if (originalUrl) {
      originalName =
        marketplaces['known-hosts'][originalUrl]?.name || originalUrl
    }

    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content: `This service was originally ${
              originalName ? 'installed from ' + originalName : 'side loaded'
            }, but you are currently connected to ${name}. To install from ${name} anyway, click "Continue".`,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }

  private async dryInstall(url: string) {
    const loader = this.loader
      .open('Checking dependent services...')
      .subscribe()

    const { id, version } = this.pkg.manifest

    try {
      const breakages = await this.embassyApi.dryUpdatePackage({
        id,
        version: `${version}`,
      })

      if (isEmptyObject(breakages)) {
        this.install(url, loader)
      } else {
        loader.unsubscribe()
        const proceed = await this.presentAlertBreakages(breakages)
        if (proceed) {
          this.install(url)
        }
      }
    } catch (e: any) {
      this.errorService.handleError(e)
    }
  }

  private alertInstall(url: string) {
    of(this.pkg.manifest.alerts.install)
      .pipe(
        switchMap(content =>
          !content
            ? of(true)
            : this.dialogs.open<boolean>(TUI_PROMPT, {
                label: 'Alert',
                size: 's',
                data: {
                  content,
                  yes: 'Install',
                  no: 'Cancel',
                },
              }),
        ),
        filter(Boolean),
      )
      .subscribe(() => this.install(url))
  }

  private async install(url: string, loader?: Subscription) {
    const message = 'Beginning Install...'

    if (loader) {
      loader.unsubscribe()
      loader.closed = false
      loader.add(this.loader.open(message).subscribe())
    } else {
      loader = this.loader.open(message).subscribe()
    }

    const { id, version } = this.pkg.manifest

    try {
      await this.marketplaceService.installPackage(id, version, url)
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }

  private async presentAlertBreakages(breakages: Breakages): Promise<boolean> {
    let content: string =
      'As a result of this update, the following services will no longer work properly and may crash:<ul>'
    const localPkgs = await getAllPackages(this.patch)
    const bullets = Object.keys(breakages).map(id => {
      const title = localPkgs[id].manifest.title
      return `<li><b>${title}</b></li>`
    })
    content = `${content}${bullets.join('')}</ul>`

    return new Promise(async resolve => {
      this.dialogs
        .open<boolean>(TUI_PROMPT, {
          label: 'Warning',
          size: 's',
          data: {
            content,
            yes: 'Continue',
            no: 'Cancel',
          },
        })
        .subscribe(response => resolve(response))
    })
  }
}
