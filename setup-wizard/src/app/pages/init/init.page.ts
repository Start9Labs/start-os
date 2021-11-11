import { Component } from '@angular/core'
import { interval, Observable } from 'rxjs'
import { finalize, take } from 'rxjs/operators'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-init',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  progress: Observable<number>
  showSuccess = false

  constructor (
    public readonly stateService: StateService,
  ) { }

  ngOnInit () {
    this.progress = interval(130)
    .pipe(
      take(101),
      finalize(() => {
        this.stateService.embassyLoaded = true
        setTimeout(() => this.download(), 500)
      }),
    )
  }

  download () {
    document.getElementById('tor-addr').innerHTML = this.stateService.torAddress
    document.getElementById('lan-addr').innerHTML = this.stateService.lanAddress
    document.getElementById('cert').setAttribute('href', 'data:application/x-x509-ca-cert;base64,' + encodeURIComponent(this.stateService.cert))
    let html = document.getElementById('downloadable').innerHTML
    const filename = 'embassy-info.html'

    const elem = document.createElement('a')
    elem.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(html))
    elem.setAttribute('download', filename)
    elem.style.display = 'none'

    document.body.appendChild(elem)
    elem.click()
    document.body.removeChild(elem)
  }
}

