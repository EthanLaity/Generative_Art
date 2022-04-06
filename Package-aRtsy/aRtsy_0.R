library(aRtsy)

artwork1 <- canvas_strokes(colors = colorPalette("neon1"))
artwork2 <- canvas_ribbons(colors = colorPalette("retro3"))
artwork3 <- canvas_forest(colors = colorPalette("retro2"))
artwork4 <- canvas_mandelbrot(colors = colorPalette("nature"))

# saveCanvas(artwork, filename = "myArtwork.png")

# plot(artwork)

grid.arrange(artwork1, artwork2, artwork3, artwork4, nrow = 2)