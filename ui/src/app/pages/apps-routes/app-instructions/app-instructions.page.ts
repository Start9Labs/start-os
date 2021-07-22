import { Component, ViewChild } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { IonContent } from '@ionic/angular'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'
import { ApiService } from 'src/app/services/api/embassy/embassy-api.service'
import { ErrorToastService } from 'src/app/services/error-toast.service'

@Component({
  selector: 'app-instructions',
  templateUrl: './app-instructions.page.html',
  styleUrls: ['./app-instructions.page.scss'],
})
export class AppInstructionsPage {
  instructions: string
  loading = true

  @ViewChild(IonContent) content: IonContent

  constructor (
    private readonly route: ActivatedRoute,
    private readonly errToast: ErrorToastService,
    private readonly embassyApi: ApiService,
    private readonly patch: PatchDbService,
  ) { }

  async ngOnInit () {
    const pkgId = this.route.snapshot.paramMap.get('pkgId')

    const url = this.patch.getData()['package-data'][pkgId]['static-files'].instructions

    try {
      this.instructions = await this.embassyApi.getStatic(url)
    } catch (e) {
      this.errToast.present(e)
    } finally {
      this.loading = false
    }
  }
}
