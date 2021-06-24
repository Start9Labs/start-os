import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { ReleaseNoteModel } from './release-notes'

@Component({
  selector: 'app-release-notes-list',
  templateUrl: './app-release-notes-list.page.html',
  styleUrls: ['./app-release-notes-list.page.scss'],
})
export class AppReleaseNotesListPage {
  error = ''
  pkgId: string
  releaseNotes: { [version: string]: string}
  selected: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    private releaseNoteModel: ReleaseNoteModel,
  ) {
      console.log('model model', releaseNoteModel.releaseNotes)
      this.releaseNotes = releaseNoteModel.releaseNotes
   }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    if (!this.releaseNotes) {
      this.getReleaseNotes()
    }
  }

  async getReleaseNotes (version?: string): Promise<void> {
    try {
      const pkg = await this.apiService.getAvailableShow({ id: this.pkgId, version })
      this.releaseNotes = pkg['release-notes']
    } catch (e) {
      console.error(e)
      this.error = e.message
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
