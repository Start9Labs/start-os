import { animate, style, transition, trigger } from '@angular/animations'

const TRANSITION = '{{duration}}ms {{delay}}ms ease-in-out'
const DURATION = { params: { duration: 300, delay: 0 } }

export const heightCollapse = trigger('heightCollapse', [
  transition(
    ':enter',
    [style({ height: 0 }), animate(TRANSITION, style({ height: '*' }))],
    DURATION,
  ),
  transition(
    ':leave',
    [style({ height: '*' }), animate(TRANSITION, style({ height: 0 }))],
    DURATION,
  ),
])
