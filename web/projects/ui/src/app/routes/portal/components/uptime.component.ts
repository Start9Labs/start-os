import { Component, Input, OnChanges, OnDestroy } from '@angular/core'
import { tuiInjectElement } from '@taiga-ui/cdk'

@Component({
  standalone: true,
  selector: '[appUptime]',
  template: '',
  styles: `
    :host-context(tui-root._mobile) {
      display: none;
    }
  `,
})
export class UptimeComponent implements OnChanges, OnDestroy {
  private readonly el = tuiInjectElement()
  private interval: any = NaN

  @Input()
  appUptime = ''

  ngOnChanges() {
    clearInterval(this.interval)

    if (!this.appUptime) {
      this.el.textContent = '-'
    } else {
      this.el.textContent = uptime(new Date(this.appUptime))
      this.interval = setInterval(() => {
        this.el.textContent = uptime(new Date(this.appUptime))
      }, 60 * 1000)
    }
  }

  ngOnDestroy() {
    clearInterval(this.interval)
  }
}

function uptime(date: Date): string {
  const delta = Date.now() - date.getTime()
  const days = Math.floor(delta / (1000 * 60 * 60 * 24))
  const hours = Math.floor((delta / (1000 * 60 * 60)) % 24)
  const minutes = Math.floor((delta / (1000 * 60)) % 60)

  if (days > 0) return `${days}d ${hours}h ${minutes}m`

  if (hours > 0) return `${hours}h ${minutes}m`

  return `${minutes}m`
}
