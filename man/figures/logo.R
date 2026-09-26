# Hex sticker for mhealth-iit (mobile-health investigator-initiated trial).
# Render from the repository root:
#   Rscript man/figures/logo.R

out <- "man/figures/logo.png"
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

keyline <- "#0B303C"
rim <- "#F4EFE6"
field <- "#135E72"
band <- "#D7E6E6"
face <- "#F7F3EB"
pulse <- "#E15D4C"
ink <- "#F7F3EB"

hex_pts <- function(cx, cy, r) {
  ang <- pi / 2 - seq(0, 5) * pi / 3
  list(x = cx + r * cos(ang), y = cy + r * sin(ang))
}

draw_hex <- function(cx, cy, r, fill) {
  p <- hex_pts(cx, cy, r)
  grid::grid.polygon(
    p$x, p$y,
    default.units = "native",
    gp = grid::gpar(fill = fill, col = NA)
  )
}

label_size <- function(label, family, face, target) {
  size <- 54
  repeat {
    w <- grid::convertWidth(
      grid::grobWidth(grid::textGrob(
        label,
        gp = grid::gpar(fontsize = size, fontfamily = family, fontface = face)
      )),
      "native",
      valueOnly = TRUE
    )
    if (w <= target || size <= 28) {
      return(size)
    }
    size <- size - 1
  }
}

grDevices::png(
  out,
  width = 6.4,
  height = 7.4,
  units = "in",
  res = 300,
  bg = "transparent",
  type = "quartz"
)

grid::grid.newpage()
grid::pushViewport(grid::viewport(
  xscale = c(0, 640),
  yscale = c(0, 740),
  clip = "off"
))

cx <- 320
cy <- 370

draw_hex(cx, cy, 312, keyline)
draw_hex(cx, cy, 300, rim)
draw_hex(cx, cy, 276, field)

# Corner radii must be npc. A native radius makes grid.roundrect fill the device.
band_r <- grid::unit(10 / 640, "npc")
face_r <- grid::unit(22 / 640, "npc")
crown_r <- grid::unit(3 / 640, "npc")

# Wristband sits behind the tracker face. The top strap stays below the
# point of the hex, where the sticker is still wide enough to hold it.
grid::grid.roundrect(
  x = 320, y = 555, width = 72, height = 36,
  default.units = "native",
  r = band_r,
  gp = grid::gpar(fill = band, col = NA)
)
grid::grid.roundrect(
  x = 320, y = 363, width = 72, height = 36,
  default.units = "native",
  r = band_r,
  gp = grid::gpar(fill = band, col = NA)
)

grid::grid.roundrect(
  x = 320, y = 459, width = 150, height = 172,
  default.units = "native",
  r = face_r,
  gp = grid::gpar(fill = face, col = keyline, lwd = 3.5)
)

# Crown on the right edge of the tracker.
grid::grid.roundrect(
  x = 402, y = 483, width = 14, height = 28,
  default.units = "native",
  r = crown_r,
  gp = grid::gpar(fill = face, col = NA)
)

# Heart-rate trace: the mobile-health signal the trial collects.
grid::grid.polyline(
  x = c(262, 300, 314, 330, 346, 360, 380),
  y = c(453, 453, 497, 419, 475, 453, 453),
  default.units = "native",
  gp = grid::gpar(col = pulse, lwd = 12, lineend = "round", linejoin = "round")
)

family <- "Avenir Next Condensed"
face <- "bold"
size <- label_size("mhealth-iit", family, face, target = 290)

grid::grid.text(
  "mhealth-iit",
  x = 320,
  y = 280,
  default.units = "native",
  gp = grid::gpar(col = ink, fontsize = size, fontfamily = family, fontface = face)
)

grDevices::dev.off()
message("Wrote ", out)
