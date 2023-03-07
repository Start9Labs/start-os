import { ChangeDetectionStrategy, Component } from '@angular/core'
import { PatchDB } from 'patch-db-client'
import { ConfigService } from 'src/app/services/config.service'
import { CopyService } from '@start9labs/shared'
import { DataModel } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'server-specs',
  templateUrl: './server-specs.page.html',
  styleUrls: ['./server-specs.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class ServerSpecsPage {
  readonly server$ = this.patch.watch$('server-info')

  constructor(
    readonly copyService: CopyService,
    private readonly patch: PatchDB<DataModel>,
    private readonly config: ConfigService,
  ) {}

  get gitHash(): string {
    return this.config.gitHash
  }

  launch(url: string): void {
    window.open(url, '_blank', 'noreferrer')
  }

  asIsOrder(a: any, b: any) {
    return 0
  }
}
