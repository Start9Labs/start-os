import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { ApiService } from 'src/app/services/api/api.service'
import { ReleaseNoteModel } from './release-notes.model'

@Component({
  selector: 'release-notes',
  templateUrl: './release-notes.page.html',
  styleUrls: ['./release-notes.page.scss'],
})
export class ReleaseNotes {
  error = ''
  pkgId: string
  selected: string

  constructor (
    private readonly route: ActivatedRoute,
    private readonly apiService: ApiService,
    public releaseNotesModel: ReleaseNoteModel,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    if (!this.releaseNotesModel.releaseNotes) {
      this.getReleaseNotes()
    }
  }

  async getReleaseNotes (version?: string): Promise<void> {
    try {
      const pkg = await this.apiService.getAvailableShow({ id: this.pkgId, version })
      this.releaseNotesModel.releaseNotes = pkg['release-notes']
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
