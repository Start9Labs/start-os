import { Component, HostListener } from '@angular/core'
import { ModalController } from '@ionic/angular'
import { PatchDbService } from 'src/app/services/patch-db/patch-db.service'

@Component({
  selector: 'snake',
  templateUrl: './snake.page.html',
  styleUrls: ['./snake.page.scss'],
})
export class SnakePage {
  speed = 6
  width = 40
  height = 26
  grid = 16

  startingLength = 4

  score = 0
  highScore = 0

  xDown: number
  yDown: number
  count = 0
  canvas: HTMLCanvasElement
  image: HTMLImageElement
  context
  snake = {
    x: this.grid * (Math.floor(this.width / 2) - this.startingLength),
    y: this.grid * Math.floor(this.height / 2),
    // snake velocity. moves one grid length every frame in either the x or y direction
    dx: this.grid,
    dy: 0,

    // keep track of all grids the snake body occupies
    cells: [],

    // length of the snake. grows when eating an apple
    maxCells: this.startingLength,
  }
  apple = {
    x: this.getRandomInt(0, this.width) * this.grid,
    y: this.getRandomInt(0, this.height) * this.grid,
  }

  moveQueue: String[] = []

  constructor(
    private readonly modalCtrl: ModalController,
    private readonly patch: PatchDbService,
  ) {}

  ngOnInit() {
    if (this.patch.data.ui.gaming?.snake?.['high-score']) {
      this.highScore = this.patch.data.ui.gaming?.snake?.['high-score']
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

  ngAfterViewInit() {
    this.canvas = document.getElementById('game') as HTMLCanvasElement
    this.canvas.width = this.grid * this.width
    this.canvas.height = this.grid * this.height
    this.context = this.canvas.getContext('2d')
    this.context.imageSmoothingEnabled = false

    this.image = new Image()
    this.image.onload = () => {
      requestAnimationFrame(() => this.loop())
    }
    this.image.src = '../../../../../../assets/img/icons/bitcoin.svg'

    // start the game
  }

  getTouches(evt: TouchEvent) {
    return evt.touches
  }

  handleTouchStart(evt) {
    const firstTouch = this.getTouches(evt)[0]
    this.xDown = firstTouch.clientX
    this.yDown = firstTouch.clientY
  }

  handleTouchMove(evt) {
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
    this.xDown = null
    this.yDown = null
  }

  // game loop
  loop() {
    requestAnimationFrame(() => this.loop())

    // slow game loop to 15 fps instead of 60 (60/15 = 4)
    if (++this.count < this.speed) {
      return
    }

    this.count = 0
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

    // wrap snake position horizontally on edge of screen
    if (this.snake.x < 0) {
      this.snake.x = this.canvas.width - this.grid
    } else if (this.snake.x >= this.canvas.width) {
      this.snake.x = 0
    }

    // wrap snake position vertically on edge of screen
    if (this.snake.y < 0) {
      this.snake.y = this.canvas.height - this.grid
    } else if (this.snake.y >= this.canvas.height) {
      this.snake.y = 0
    }

    // keep track of where snake has been. front of the array is always the head
    this.snake.cells.unshift({ x: this.snake.x, y: this.snake.y })

    // remove cells as we move away from them
    if (this.snake.cells.length > this.snake.maxCells) {
      this.snake.cells.pop()
    }

    // draw apple
    this.context.fillStyle = '#ff4961'
    this.context.drawImage(
      this.image,
      this.apple.x - 2,
      this.apple.y - 2,
      this.grid + 4,
      this.grid + 4,
    )

    // draw snake one cell at a time
    this.context.fillStyle = '#2fdf75'

    const firstCell = this.snake.cells[0]

    for (let index = 0; index < this.snake.cells.length; index++) {
      const cell = this.snake.cells[index]

      // drawing 1 px smaller than the grid creates a grid effect in the snake body so you can see how long it is
      this.context.fillRect(cell.x, cell.y, this.grid - 1, this.grid - 1)

      // snake ate apple
      if (cell.x === this.apple.x && cell.y === this.apple.y) {
        this.score++
        if (this.score > this.highScore) this.highScore = this.score
        this.snake.maxCells++

        this.apple.x = this.getRandomInt(0, this.width) * this.grid
        this.apple.y = this.getRandomInt(0, this.height) * this.grid
      }

      if (index > 0) {
        // check collision with all cells after this one (modified bubble sort)
        // snake occupies same space as a body part. reset game
        if (
          firstCell.x === this.snake.cells[index].x &&
          firstCell.y === this.snake.cells[index].y
        ) {
          this.snake.x =
            this.grid * (Math.floor(this.width / 2) - this.startingLength)
          this.snake.y = this.grid * Math.floor(this.height / 2)
          this.snake.cells = []
          this.snake.maxCells = this.startingLength
          this.snake.dx = this.grid
          this.snake.dy = 0

          this.apple.x = this.getRandomInt(0, 25) * this.grid
          this.apple.y = this.getRandomInt(0, 25) * this.grid
          this.score = 0
        }
      }
    }
  }

  getRandomInt(min, max) {
    return Math.floor(Math.random() * (max - min)) + min
  }
}
