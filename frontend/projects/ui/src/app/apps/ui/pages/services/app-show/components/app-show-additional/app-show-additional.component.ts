import { ChangeDetectionStrategy, Component, Input } from '@angular/core'
import { CopyService, MarkdownComponent } from '@start9labs/shared'
import { TuiDialogService } from '@taiga-ui/core'
import { PolymorpheusComponent } from '@tinkoff/ng-polymorpheus'
import { from } from 'rxjs'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { PackageDataEntry } from 'src/app/services/patch-db/data-model'

@Component({
  selector: 'app-show-additional',
  templateUrl: 'app-show-additional.component.html',
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class AppShowAdditionalComponent {
  @Input()
  pkg!: PackageDataEntry

  constructor(
    readonly copyService: CopyService,
    private readonly dialogs: TuiDialogService,
    private readonly api: ApiService,
  ) {}

  presentModalLicense() {
    const { id, version } = this.pkg.manifest

    this.dialogs
      .open(new PolymorpheusComponent(MarkdownComponent), {
        label: 'License',
        size: 'l',
        data: {
          content: from(
            this.api.getStatic(
              `/public/package-data/${id}/${version}/LICENSE.md`,
            ),
          ),
        },
      })
      .subscribe()
  }
}
