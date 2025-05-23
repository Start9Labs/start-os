import { Component, ElementRef, inject, NgZone, OnInit } from '@angular/core'
import { WA_WINDOW } from '@ng-web-apis/common'

// a higher fade factor will make the characters fade quicker
const FADE_FACTOR = 0.07

@Component({
  standalone: true,
  selector: 'canvas[matrix]',
  template: 'Your browser does not support the canvas element.',
  styles: ':host { position: fixed; top: 0 }',
})
export class MatrixComponent implements OnInit {
  private readonly ngZone = inject(NgZone)
  private readonly window = inject(WA_WINDOW)
  private readonly el: HTMLCanvasElement = inject(ElementRef).nativeElement
  private readonly ctx = this.el.getContext('2d')!

  private tileSize = 16
  private columns: any[] = []
  private maxStackHeight = 0

  ngOnInit() {
    this.ngZone.runOutsideAngular(() => {
      this.setupMatrixGrid()
      this.tick()
    })
  }

  private setupMatrixGrid() {
    this.el.width = Math.max(this.window.innerWidth, 1920)
    this.el.height = Math.max(this.window.innerHeight, 1080)
    this.maxStackHeight = Math.ceil(this.ctx.canvas.height / this.tileSize)
    // divide the canvas into columns
    for (let i = 0; i < this.ctx.canvas.width / this.tileSize; ++i) {
      const column = {} as any
      // save the x position of the column
      column.x = i * this.tileSize
      // create a random stack height for the column
      column.stackHeight = 10 + Math.random() * this.maxStackHeight
      // add a counter to count the stack height
      column.stackCounter = 0
      // add the column to the list
      this.columns.push(column)
    }
  }

  private draw() {
    // draw a semi transparent black rectangle on top of the scene to slowly fade older characters
    this.ctx.fillStyle = `rgba(0, 0, 0, ${FADE_FACTOR})`
    this.ctx.fillRect(0, 0, this.ctx.canvas.width, this.ctx.canvas.height)
    // pick a font slightly smaller than the tile size
    this.ctx.font = `${this.tileSize - 2}px monospace`
    this.ctx.fillStyle = '#ff4961'
    for (let i = 0; i < this.columns.length; ++i) {
      // pick a random ascii character (change the 94 to a higher number to include more characters)
      const { x, stackCounter } = this.columns[i]
      const char = String.fromCharCode(33 + Math.floor(Math.random() * 94))
      this.ctx.fillText(char, x, stackCounter * this.tileSize + this.tileSize)
      // if the stack is at its height limit, pick a new random height and reset the counter
      if (++this.columns[i].stackCounter >= this.columns[i].stackHeight) {
        this.columns[i].stackHeight = 10 + Math.random() * this.maxStackHeight
        this.columns[i].stackCounter = 0
      }
    }
  }

  private tick() {
    this.draw()
    setTimeout(this.tick.bind(this), 50)
  }
}
