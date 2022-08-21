import { ChangeDetectionStrategy, Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { BasicInfo, getBasicInfoSpec } from './form-info'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService } from '@start9labs/shared'
import { getProjectId } from 'src/app/util/get-project-id'
import { DevProjectData } from 'src/app/services/patch-db/data-model'

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
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly patch: PatchDbService,
  ) {}

  async openBasicInfoModal(data: DevProjectData) {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Basic Info',
        spec: getBasicInfoSpec(data),
        buttons: [
          {
            text: 'Save',
            handler: (basicInfo: BasicInfo) => {
              this.saveBasicInfo(basicInfo)
            },
            isSubmit: true,
          },
        ],
      },
    })
    await modal.present()
  }

  async saveBasicInfo(basicInfo: BasicInfo) {
    const loader = await this.loadingCtrl.create({
      message: 'Saving...',
    })
    await loader.present()

    try {
      await this.api.setDbValue({
        pointer: `/dev/${this.projectId}/basic-info`,
        value: basicInfo,
      })
    } catch (e: any) {
      this.errToast.present(e)
    } finally {
      loader.dismiss()
    }
  }
}
