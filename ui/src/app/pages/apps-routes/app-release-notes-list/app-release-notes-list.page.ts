import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { PackageDataEntry } from 'src/app/models/patch-db/data-model'
import { AvailableShow } from 'src/app/services/api/api-types'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  selector: 'app-release-notes-list',
  templateUrl: './app-release-notes-list.page.html',
  styleUrls: ['./app-release-notes-list.page.scss'],
})
export class AppReleaseNotesList {
  loading = true
  pkgId: string
  pkg: AvailableShow

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,

  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
  }

  async getPkg (version?: string): Promise<void> {
    this.loading = true
    try {
      this.pkg = await this.apiService.getAvailableShow({ id: this.pkgId, version })
    } catch (e) {
      console.error(e)
      this.error = e.message
    } finally {
      this.loading = false
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
