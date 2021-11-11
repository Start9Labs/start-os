import { Component } from '@angular/core'
import { interval, Observable, Subscription } from 'rxjs'
import { delay, finalize, take, tap } from 'rxjs/operators'
import { StateService } from 'src/app/services/state.service'

@Component({
  selector: 'app-init',
  templateUrl: 'init.page.html',
  styleUrls: ['init.page.scss'],
})
export class InitPage {
  progress: number
  sub: Subscription

  constructor (
    public readonly stateService: StateService,
  ) { }

  ngOnInit () {
    this.sub = interval(130)
    .pipe(
      take(101),
      tap(num => {
        this.progress = num
      }),
      finalize(() => {
        setTimeout(() => {
          this.stateService.embassyLoaded = true
          this.download()
        }, 500)
      }),
    ).subscribe()
  }

  ngOnDestroy () {
    if (this.sub) this.sub.unsubscribe()
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

