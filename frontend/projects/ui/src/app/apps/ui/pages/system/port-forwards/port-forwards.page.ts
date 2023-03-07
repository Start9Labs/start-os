import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { DataModel, PortForward } from 'src/app/services/patch-db/data-model'
import { LoadingService, CopyService, ErrorService } from '@start9labs/shared'
import { ApiService } from 'src/app/services/api/embassy-api.service'

@Component({
  selector: 'port-forwards',
  templateUrl: './port-forwards.page.html',
  styleUrls: ['./port-forwards.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class PortForwardsPage {
  readonly server$ = this.patch.watch$('server-info')
  editing: Record<string, boolean> = {}
  overrides: Record<string, number> = {}

  constructor(
    readonly copyService: CopyService,
    private readonly patch: PatchDB<DataModel>,
    private readonly loader: LoadingService,
    private readonly errorService: ErrorService,
    private readonly api: ApiService,
  ) {}

  async editPort(pf: PortForward) {
    this.editing[pf.target] = !this.editing[pf.target]
    this.overrides[pf.target] = pf.override || pf.assigned
  }

  async saveOverride(pf: PortForward) {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.overridePortForward({
        target: pf.target,
        port: this.overrides[pf.target],
      })
      delete this.editing[pf.target]
    } catch (e: any) {
      this.errorService.handleError(e)
    } finally {
      loader.unsubscribe()
    }
  }
}
