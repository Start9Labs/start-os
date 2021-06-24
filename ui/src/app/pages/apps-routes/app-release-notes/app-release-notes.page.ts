import { Component } from '@angular/core'
import { ActivatedRoute } from '@angular/router'
import { AppAvailableService } from '../app-available.service'

@Component({
  selector: 'app-release-notes',
  templateUrl: './app-release-notes.page.html',
  styleUrls: ['./app-release-notes.page.scss'],
})
export class AppReleaseNotes {
  error = ''
  selected: string
  pkgId: string

  constructor (
    private readonly route: ActivatedRoute,
    public aaService: AppAvailableService,
  ) { }

  ngOnInit () {
    this.pkgId = this.route.snapshot.paramMap.get('pkgId')
    const version = this.route.snapshot.paramMap.get('version')
    if (!this.aaService.pkgs[this.pkgId]) {
      this.aaService.setPkg(this.pkgId, version)
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
