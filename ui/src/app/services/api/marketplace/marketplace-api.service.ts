import { RR } from '../api.types'
import { ConfigService } from '../../config.service'
import { PatchDbService } from '../../patch-db/patch-db.service'
import { ServerInfo } from '../../patch-db/data-model'
import { AuthState } from '../../auth.service'
import { takeWhile } from 'rxjs/operators'

export abstract class MarketplaceApiService {
  private server: ServerInfo

  constructor (
    readonly config: ConfigService,
    readonly patch: PatchDbService,
  ) { }

  init (auth: AuthState) {
    this.patch.watch$('server-info')
    .pipe(
      takeWhile(() => auth === AuthState.VERIFIED),
    )
    .subscribe(server => {
      this.server = server
    })
  }

  abstract getEos (params: RR.GetMarketplaceEOSReq): Promise<RR.GetMarketplaceEOSRes>

  abstract getMarketplaceData (params: RR.GetMarketplaceDataReq): Promise<RR.GetMarketplaceDataRes>

  abstract getMarketplacePkgs (params: RR.GetMarketplacePackagesReq): Promise<RR.GetMarketplacePackagesRes>

  abstract getReleaseNotes (params: RR.GetReleaseNotesReq): Promise<RR.GetReleaseNotesRes>

  abstract getLatestVersion (params: RR.GetLatestVersionReq): Promise<RR.GetLatestVersionRes>
}