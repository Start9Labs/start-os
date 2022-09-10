import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { BasicInfo, getBasicInfoSpec } from './form-info'
import { PatchDB } from 'patch-db-client'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorService } from '@start9labs/shared'
import { getProjectId } from 'src/app/util/get-project-id'
import { DataModel, DevProjectData } from 'src/app/services/patch-db/data-model'
import { FormDialogService } from '../../../services/form-dialog.service'
import { FormPage } from '../../../modals/form/form.page'
import { LoadingService } from '../../../modals/loading/loading.service'

@Component({
  selector: 'developer-menu',
  templateUrl: 'developer-menu.page.html',
  styleUrls: ['developer-menu.page.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush,
})
export class DeveloperMenuPage {
  readonly projectId = getProjectId(this.route)
  readonly projectData$ = this.patch.watch$('ui', 'dev', this.projectId)

  constructor(
    private readonly formDialog: FormDialogService,
    private readonly loader: LoadingService,
    private readonly errorHandler: ErrorService,
    private readonly route: ActivatedRoute,
    private readonly api: ApiService,
    private readonly patch: PatchDB<DataModel>,
  ) {}

  async openBasicInfoModal(data: DevProjectData) {
    this.formDialog.open(FormPage, {
      label: 'Basic Info',
      data: {
        spec: getBasicInfoSpec(data),
        buttons: [
          {
            text: 'Save',
            handler: async (basicInfo: BasicInfo) =>
              this.saveBasicInfo(basicInfo),
          },
        ],
      },
    })
  }

  async saveBasicInfo(basicInfo: BasicInfo): Promise<boolean> {
    const loader = this.loader.open('Saving...').subscribe()

    try {
      await this.api.setDbValue<BasicInfo>(
        ['dev', this.projectId, 'basic-info'],
        basicInfo,
      )
      return true
    } catch (e: any) {
      this.errorHandler.handleError(e)
      return false
    } finally {
      loader.unsubscribe()
    }
  }
}