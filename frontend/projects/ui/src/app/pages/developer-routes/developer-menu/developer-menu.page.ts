import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { LoadingController, ModalController } from '@ionic/angular'
import { GenericFormPage } from 'src/app/modals/generic-form/generic-form.page'
import { BasicInfo, getBasicInfoSpec } from './form-info'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/embassy-api.service'
import { ErrorToastService, DestroyService } from '@start9labs/shared'
import { takeUntil } from 'rxjs/operators'
import { DevProjectData } from 'src/app/services/patch-db/data-model'
import { getProjectId } from 'src/app/util/get-project-id'
import * as yaml from 'js-yaml'

@Component({
  selector: 'developer-menu',
  templateUrl: 'developer-menu.page.html',
  styleUrls: ['developer-menu.page.scss'],
  providers: [DestroyService],
})
export class DeveloperMenuPage {
  readonly projectId = getProjectId(this.route)
  projectData: DevProjectData

  constructor(
    private readonly route: ActivatedRoute,
    private readonly modalCtrl: ModalController,
    private readonly loadingCtrl: LoadingController,
    private readonly api: ApiService,
    private readonly errToast: ErrorToastService,
    private readonly destroy$: DestroyService,
    private readonly patchDb: PatchDbService,
  ) {}

  get name(): string {
    return this.patchDb.getData().ui?.dev?.[this.projectId]?.name || ''
  }

  ngOnInit() {
    this.patchDb
      .watch$('ui', 'dev', this.projectId)
      .pipe(takeUntil(this.destroy$))
      .subscribe(pd => {
        this.projectData = pd
      })
  }

  async openBasicInfoModal() {
    const modal = await this.modalCtrl.create({
      component: GenericFormPage,
      componentProps: {
        title: 'Basic Info',
        spec: getBasicInfoSpec(this.projectData),
        buttons: [
          {
            text: 'Save',
            handler: (basicInfo: any) => {
              basicInfo.description = {
                short: basicInfo.short,
                long: basicInfo.long,
              }
              delete basicInfo.short
              delete basicInfo.long
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
