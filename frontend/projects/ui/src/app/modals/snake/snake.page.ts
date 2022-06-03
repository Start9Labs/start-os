import { Component, HostListener } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { pauseFor } from '@start9labs/shared'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'snake',
  templateUrl: './snake.page.html',
  styleUrls: ['./snake.page.scss'],
})
export class SnakePage {
  speed = 45
  width = 40
  height = 26
  grid = NaN

  startingLength = 4

  score = 0
  highScore = 0

  xDown?: number
  yDown?: number
  canvas: HTMLCanvasElement
  image: HTMLImageElement
  context: CanvasRenderingContext2D

  snake: any
  bitcoin: { x: number; y: number } = { x: NaN, y: NaN }

  moveQueue: String[] = []

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    if (this.patch.getData().ui.gaming?.snake?.['high-score']) {
      this.highScore =
        this.patch.getData().ui.gaming?.snake?.['high-score'] || 0
    }
  }

  async dismiss() {
    return this.modalCtrl.dismiss({ highScore: this.highScore })
  }

  @HostListener('document:keydown', ['$event'])
  keyEvent(e: KeyboardEvent) {
    this.moveQueue.push(e.key)
  }

  @HostListener('touchstart', ['$event'])
  touchStart(e: TouchEvent) {
    this.handleTouchStart(e)
  }

  @HostListener('touchmove', ['$event'])
  touchMove(e: TouchEvent) {
    this.handleTouchMove(e)
  }

  @HostListener('window:resize')
  sizeChange() {
    this.init()
  }

  ionViewDidEnter() {
    this.init()

    this.image = new Image()
    this.image.onload = () => {
      requestAnimationFrame(async () => await this.loop())
    }
    this.image.src = '../../../../../../assets/img/icons/bitcoin.svg'
  }

  init() {
    this.canvas = document.getElementById('game') as HTMLCanvasElement
    this.canvas.style.border = '1px solid #e0e0e0'
    this.context = this.canvas.getContext('2d')!
    const container = document.getElementsByClassName('canvas-center')[0]
    this.grid = Math.min(
      Math.floor(container.clientWidth / this.width),
      Math.floor(container.clientHeight / this.height),
    )
    this.snake = {
      x: this.grid * (Math.floor(this.width / 2) - this.startingLength),
      y: this.grid * Math.floor(this.height / 2),
      // snake velocity. moves one grid length every frame in either the x or y direction
      dx: this.grid,
      dy: 0,
      // keep track of all grids the snake body occupies
      cells: [],
      // length of the snake. grows when eating an bitcoin
      maxCells: this.startingLength,
    }
    this.bitcoin = {
      x: this.getRandomInt(0, this.width) * this.grid,
      y: this.getRandomInt(0, this.height) * this.grid,
    }

    this.canvas.width = this.grid * this.width
    this.canvas.height = this.grid * this.height
    this.context.imageSmoothingEnabled = false
  }

  getTouches(evt: TouchEvent) {
    return evt.touches
  }

  handleTouchStart(evt: TouchEvent) {
    const firstTouch = this.getTouches(evt)[0]
    this.xDown = firstTouch.clientX
    this.yDown = firstTouch.clientY
  }

  handleTouchMove(evt: TouchEvent) {
    if (!this.xDown || !this.yDown) {
      return
    }

    var xUp = evt.touches[0].clientX
    var yUp = evt.touches[0].clientY

    var xDiff = this.xDown - xUp
    var yDiff = this.yDown - yUp

    if (Math.abs(xDiff) > Math.abs(yDiff)) {
      /*most significant*/
      if (xDiff > 0) {
        this.moveQueue.push('ArrowLeft')
      } else {
        this.moveQueue.push('ArrowRight')
      }
    } else {
      if (yDiff > 0) {
        this.moveQueue.push('ArrowUp')
      } else {
        this.moveQueue.push('ArrowDown')
      }
    }
    /* reset values */
    this.xDown = undefined
    this.yDown = undefined
  }

  // game loop
  async loop() {
    await pauseFor(this.speed)

    requestAnimationFrame(async () => await this.loop())

    this.context.clearRect(0, 0, this.canvas.width, this.canvas.height)

    // move snake by it's velocity
    this.snake.x += this.snake.dx
    this.snake.y += this.snake.dy

    if (this.moveQueue.length) {
      const move = this.moveQueue.shift()
      // left arrow key
      if (move === 'ArrowLeft' && this.snake.dx === 0) {
        this.snake.dx = -this.grid
        this.snake.dy = 0
      }
      // up arrow key
      else if (move === 'ArrowUp' && this.snake.dy === 0) {
        this.snake.dy = -this.grid
        this.snake.dx = 0
      }
      // right arrow key
      else if (move === 'ArrowRight' && this.snake.dx === 0) {
        this.snake.dx = this.grid
        this.snake.dy = 0
      }
      // down arrow key
      else if (move === 'ArrowDown' && this.snake.dy === 0) {
        this.snake.dy = this.grid
        this.snake.dx = 0
      }
    }

    // edge death
    if (
      this.snake.x < 0 ||
      this.snake.y < 0 ||
      this.snake.x >= this.canvas.width ||
      this.snake.y >= this.canvas.height
    ) {
      this.death()
    }

    // keep track of where snake has been. front of the array is always the head
    this.snake.cells.unshift({ x: this.snake.x, y: this.snake.y })

    // remove cells as we move away from them
    if (this.snake.cells.length > this.snake.maxCells) {
      this.snake.cells.pop()
    }

    // draw bitcoin
    this.context.fillStyle = '#ff4961'
    this.context.drawImage(
      this.image,
      this.bitcoin.x - 1,
      this.bitcoin.y - 1,
      this.grid + 2,
      this.grid + 2,
    )

    // draw snake one cell at a time
    this.context.fillStyle = '#2fdf75'

    const firstCell = this.snake.cells[0]

    for (let index = 0; index < this.snake.cells.length; index++) {
      const cell = this.snake.cells[index]

      // drawing 1 px smaller than the grid creates a grid effect in the snake body so you can see how long it is
      this.context.fillRect(cell.x, cell.y, this.grid - 1, this.grid - 1)

      // snake ate bitcoin
      if (cell.x === this.bitcoin.x && cell.y === this.bitcoin.y) {
        this.score++
        if (this.score > this.highScore) this.highScore = this.score
        this.snake.maxCells++

        this.bitcoin.x = this.getRandomInt(0, this.width) * this.grid
        this.bitcoin.y = this.getRandomInt(0, this.height) * this.grid
      }

      if (index > 0) {
        // check collision with all cells after this one (modified bubble sort)
        // snake occupies same space as a body part. reset game
        if (
          firstCell.x === this.snake.cells[index].x &&
          firstCell.y === this.snake.cells[index].y
        ) {
          this.death()
        }
      }
    }
  }

  death() {
    this.snake.x =
      this.grid * (Math.floor(this.width / 2) - this.startingLength)
    this.snake.y = this.grid * Math.floor(this.height / 2)
    this.snake.cells = []
    this.snake.maxCells = this.startingLength
    this.snake.dx = this.grid
    this.snake.dy = 0

    this.bitcoin.x = this.getRandomInt(0, 25) * this.grid
    this.bitcoin.y = this.getRandomInt(0, 25) * this.grid
    this.score = 0
  }

  getRandomInt(min: number, max: number) {
    return Math.floor(Math.random() * (max - min)) + min
  }
}
