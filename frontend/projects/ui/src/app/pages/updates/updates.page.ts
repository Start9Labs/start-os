import { Component, Inject } from '@angular/core'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PatchDB } from 'patch-db-client'
import {
  DataModel,
  PackageDataEntry,
  PackageState,
} from 'src/app/services/patch-db/data-model'
import { MarketplaceService } from 'src/app/services/marketplace.service'
import {
  AbstractMarketplaceService,
  Marketplace,
  MarketplaceManifest,
  MarketplacePkg,
} from '@start9labs/marketplace'
import { Emver } from '@start9labs/shared'
import { Pipe, PipeTransform } from '@angular/core'
import { combineLatest, Observable } from 'rxjs'
import { PrimaryRendering } from '../../services/pkg-status-rendering.service'

interface UpdatesData {
  hosts: Record<string, string>
  marketplace: Marketplace
  localPkgs: Record<string, PackageDataEntry>
  errors: string[]
}

@Component({
  selector: 'updates',
  templateUrl: 'updates.page.html',
  styleUrls: ['updates.page.scss'],
})
export class UpdatesPage {
  queued: Record<string, boolean> = {}

  readonly data$: Observable<UpdatesData> = combineLatest({
    hosts: this.marketplaceService.getKnownHosts$(),
    marketplace: this.marketplaceService.getMarketplace$(),
    localPkgs: this.patch.watch$('package-data'),
    errors: this.marketplaceService.getRequestErrors$(),
  })

  readonly PackageState = PackageState
  readonly rendering = PrimaryRendering[PackageState.Installing]

  constructor(
    @Inject(AbstractMarketplaceService)
    private readonly marketplaceService: MarketplaceService,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async update(id: string, url: string): Promise<void> {
    this.queued[id] = true
    this.api.installPackage({ id, 'marketplace-url': url })
  }
}

@Pipe({
  name: 'filterUpdates',
})
export class FilterUpdatesPipe implements PipeTransform {
  constructor(private readonly emver: Emver) {}

  transform(
    pkgs: MarketplacePkg[],
    local: Record<string, PackageDataEntry> = {},
    url: string,
  ): MarketplacePkg[] {
    return pkgs.filter(
      ({ manifest }) =>
        marketplaceSame(manifest, local, url) &&
        versionLower(manifest, local, this.emver),
    )
  }
}

export function marketplaceSame(
  { id }: MarketplaceManifest,
  local: Record<string, PackageDataEntry>,
  url: string,
): boolean {
  return local[id]?.installed?.['marketplace-url'] === url
}

export function versionLower(
  { version, id }: MarketplaceManifest,
  local: Record<string, PackageDataEntry>,
  emver: Emver,
): boolean {
  return (
    local[id].state === PackageState.Installing ||
    emver.compare(version, local[id].installed?.manifest.version || '') === 1
  )
}
