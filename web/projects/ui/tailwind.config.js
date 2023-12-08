/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./src/**/*.{html,ts}', '../marketplace/src/**/*.{html,ts}'],
  theme: {
    extend: {
      keyframes: {
        shine: {
          '100%': { left: '125%' },
        },
      },
      screens: {
        xs: '370px',
        '3xl': '1792px',
        '4xl': '2048px',
      },
      colors: {
        folly: '#FF4961',
        lime: '#BCFF49',
        cyan: '#49FFE7',
        veronica: '#9747ff',
        tangerine: '#ff8c49',
        royal: '#4961ff',
        background: '#222428',
      },
    },
    fontFamily: {
      sans: ['Montserrat', 'sans-serif'],
    },
  },
}
