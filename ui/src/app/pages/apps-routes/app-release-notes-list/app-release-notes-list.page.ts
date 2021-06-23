import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AvailableShow } from 'src/app/services/api/api-types'
import { ApiService } from 'src/app/services/api/api.service'

@Component({
  selector: 'app-release-notes-list',
  templateUrl: './app-release-notes-list.page.html',
  styleUrls: ['./app-release-notes-list.page.scss'],
})
export class AppReleaseNotesListPage {
  loading = true
  error = ''
  pkgId: string
  pkg: AvailableShow
  selected: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,

  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    this.getPkg()
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

  setSelected (selected: string) {
    if (this.selected === selected) {
      this.selected = null
    } else {
      this.selected = selected
    }
  }

  asIsOrder (a: any, b: any) {
    return 0
  }
}
