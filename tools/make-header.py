#!/usr/bin/env python3
"""make-header.py -- draw the header artwork: two of the game's paintings on
their plates, a few cubes gone, a large ant crossing between them, and a
line of small ants marching off with the missing cubes, under the title.

Each layout is laid out on a board 800 high and as wide as its aspect, and
drawn at its full size, so edges stay sharp:

    banner     2400x800    docs/header.png, the web page's banner
    wide       3840x1646   docs/header-3840x1646.png, the App Store
    sixteen    5244x2950   docs/header-5244x2950.png, the App Store (16:9)

The pictures are read from src/model.lisp.  The ants are drawn as in
make-icon.py, from signed distance functions; the title is set in Avenir
Next by tools/header-title.swift.  The big ones take a few minutes.

    python3 tools/make-header.py [layout ...]
"""
import json, math, os, re, struct, subprocess, sys, tempfile, zlib

here = os.path.dirname(os.path.abspath(__file__))
root = os.path.normpath(os.path.join(here, ".."))

PALETTE = {c: tuple(round(v * 255) for v in rgb) for c, rgb in {
    "R": (0.91, 0.24, 0.22), "O": (0.98, 0.55, 0.16), "Y": (0.99, 0.84, 0.22),
    "G": (0.33, 0.72, 0.30), "C": (0.45, 0.82, 0.93), "B": (0.20, 0.45, 0.90),
    "P": (0.96, 0.50, 0.74), "T": (0.95, 0.84, 0.66), "N": (0.52, 0.33, 0.19),
    "W": (0.97, 0.97, 0.95), "K": (0.20, 0.18, 0.22)}.items()}
GROUND = (66, 87, 56)
PLATE = (204, 199, 189)
PLATE_EDGE = (140, 135, 128)
TAN = "#F2D6A8"

# Board coordinates, 800 high.  title: x (or "centre"), then each line's
# baseline and size; paintings: cell size, and each one's left and top;
# march: the line of ants' start and end x, y, and scale; ant: the big one's
# centre, heading and scale.
LAYOUTS = {
    "banner": dict(size=(2400, 800), out="docs/header.png",
                   title=dict(x=112, lines=[(390, 260), (500, 86), (585, 42)]),
                   cell=19, paintings=[(1060, 150), (1700, 150)],
                   march=(2080, 780, 712, 0.135), ant=(1665, 420, 32, 0.42)),
    "wide": dict(size=(3840, 1646), out="docs/header-3840x1646.png",
                 title=dict(x=100, lines=[(330, 230), (432, 78), (505, 38), (553, 38)]),
                 cell=16, paintings=[(730, 150), (1270, 150)],
                 march=(1720, 230, 690, 0.125), ant=(1240, 395, 32, 0.36)),
    "sixteen": dict(size=(5244, 2950), out="docs/header-5244x2950.png",
                    title=dict(x="centre", lines=[(215, 180), (295, 62), (350, 30)]),
                    cell=11, paintings=[(360, 400), (730, 400)],
                    march=(1300, 120, 745, 0.1), ant=(710, 520, 32, 0.28)),
}

def pictures():
    source = open(os.path.join(root, "src", "model.lisp")).read()
    found = {}
    for m in re.finditer(r':name "([^"]+)".*?:rows \'\(([^)]*)\)', source, re.S):
        found[m.group(1)] = re.findall(r'"([^"]*)"', m.group(2))
    return found

def shade(colour, k):
    return tuple(max(0, min(255, c * k)) for c in colour)

def mix(a, b, t):
    return tuple(x + (y - x) * t for x, y in zip(a, b))

# ------------------------------------------------------------------
# ants: in the ant's frame u runs to the head, v across; sizes are for an
# ant about 950 long, scaled

BODY = [(-235, 0, 205, 150), (-35, 0, 38, 30), (55, 0, 105, 72), (215, 0, 98, 92)]
LIMBS = []
for side in (-1, 1):
    for root_u, knee, foot in ((95, (175, 205), (300, 330)),
                               (55, (40, 240), (-10, 380)),
                               (15, (-110, 205), (-250, 330))):
        LIMBS.append(((root_u, side * 40), (knee[0], side * knee[1]), 17))
        LIMBS.append(((knee[0], side * knee[1]), (foot[0], side * foot[1]), 13))
    LIMBS.append(((280, side * 45), (360, side * 135), 8))
    LIMBS.append(((360, side * 135), (475, side * 120), 7))
    LIMBS.append(((285, side * 45), (330, side * 40), 13))
CARRIED = (372, 0, 58, 12)

def ellipse(u, v, cu, cv, ru, rv):
    nu, nv = (u - cu) / ru, (v - cv) / rv
    return (math.hypot(nu, nv) - 1) * min(ru, rv), (nu, nv)

def capsule(u, v, a, b, r):
    pu, pv = u - a[0], v - a[1]
    du, dv = b[0] - a[0], b[1] - a[1]
    t = max(0, min(1, (pu * du + pv * dv) / (du * du + dv * dv)))
    return math.hypot(pu - du * t, pv - dv * t) - r

def box(u, v, cu, cv, half, radius):
    qu, qv = abs(u - cu) - half + radius, abs(v - cv) - half + radius
    return math.hypot(max(qu, 0), max(qv, 0)) + min(max(qu, qv), 0) - radius

class Canvas:
    def __init__(self, width, height):
        self.W, self.H = width, height
        self.k = height / 800                   # pixels per board unit
        self.pixels = []
        aspect = width / height
        for y in range(height):
            dy = (y / height - 0.5) ** 2
            self.pixels.append([shade(GROUND, 1.08 - 0.45 * (((x / width - 0.55) * aspect / 3) ** 2 + dy))
                                for x in range(width)])

    def fill(self, x0, y0, x1, y1, colour):
        k = self.k
        a, b = max(0, round(x0 * k)), min(self.W, round(x1 * k))
        if b <= a:
            return
        for y in range(max(0, round(y0 * k)), min(self.H, round(y1 * k))):
            self.pixels[y][a:b] = [colour] * (b - a)

    def darken(self, x0, y0, x1, y1, f):
        k = self.k
        for y in range(max(0, round(y0 * k)), min(self.H, round(y1 * k))):
            row = self.pixels[y]
            for x in range(max(0, round(x0 * k)), min(self.W, round(x1 * k))):
                row[x] = shade(row[x], f)

    def plate(self, rows, left, top, cell, missing=()):
        """A picture on its plate, as in the game; MISSING lists (row, column)s taken."""
        w, h = len(rows[0]), len(rows)
        pad = cell * 0.45
        self.darken(left - pad + 12, top - pad + 16, left + w * cell + pad + 12, top + h * cell + pad + 16, 0.72)
        self.fill(left - pad - 3, top - pad - 3, left + w * cell + pad + 3, top + h * cell + pad + 3, PLATE_EDGE)
        self.fill(left - pad, top - pad, left + w * cell + pad, top + h * cell + pad, PLATE)
        gap = cell * 0.08
        for r, line in enumerate(rows):
            for c, char in enumerate(line):
                if char == "." or (r, c) in missing:
                    continue
                x, y, s = left + c * cell + gap, top + r * cell + gap, cell - 2 * gap
                colour = PALETTE[char]
                self.fill(x, y, x + s, y + s, colour)
                self.fill(x, y, x + s, y + s * 0.22, shade(colour, 1.15) if char != "W" else (255, 255, 255))
                self.fill(x, y + s * 0.84, x + s, y + s, shade(colour, 0.78))

    def ant(self, cx, cy, degrees, scale, colour, cube_colour):
        """An ant centred at board (CX, CY), heading DEGREES (0 right, counterclockwise
        up), SCALE times the icon's, carrying a cube of CUBE_COLOUR."""
        k = self.k
        cx, cy, scale = cx * k, cy * k, scale * k
        a = math.radians(-degrees)
        cu, su = math.cos(a), math.sin(a)
        dark = shade(colour, 0.42)
        lu, lv = -cu - su, su - cu
        n = math.hypot(lu, lv)
        light = (lu / n * 0.65, lv / n * 0.65, 0.76)
        def at(x, y):
            dx, dy = (x - cx) / scale, (y - cy) / scale
            u, v = dx * cu + dy * su, -dx * su + dy * cu
            if not (-500 < u < 540 and -440 < v < 440):
                return 1e9, None
            best, col = 1e9, None
            for bu, bv, ru, rv in BODY:
                d, (nu, nv) = ellipse(u, v, bu, bv, ru, rv)
                if d < best:
                    nz = math.sqrt(max(0.0, 1 - min(1.0, nu * nu + nv * nv)))
                    lit = max(0.0, nu * light[0] + nv * light[1] + nz * light[2])
                    base = mix(dark, colour, min(1.0, 0.25 + 0.95 * lit))
                    spec = max(0.0, lit - 0.82) / 0.18
                    best, col = d, mix(base, (255, 240, 235), spec * 0.7)
            for p, q, r in LIMBS:
                d = capsule(u, v, p, q, r)
                if d < best:
                    best, col = d, shade(dark, 0.8)
            d = box(u, v, *CARRIED)
            if d < best:
                best, col = d, (shade(cube_colour, 1.1) if v < -CARRIED[2] * 0.45
                                else shade(cube_colour, 0.8) if v > CARRIED[2] * 0.6 else cube_colour)
            return best * scale, col
        reach = 540 * scale + 40 * k
        sx, sy, soft = 16 * scale + 3 * k, 20 * scale + 4 * k, 30 * scale + 4 * k
        edge = 4.5 * scale + 1.2 * k
        for y in range(max(0, int(cy - reach)), min(self.H, int(cy + reach))):
            row = self.pixels[y]
            for x in range(max(0, int(cx - reach)), min(self.W, int(cx + reach))):
                d, _ = at(x - sx, y - sy)
                if d < soft:
                    row[x] = shade(row[x], 1 - 0.45 * max(0.0, min(1.0, (soft - d) / (soft * 1.3))))
                d, col = at(x, y)
                outline = max(0.0, min(1.0, edge - d))
                if outline > 0:
                    row[x] = mix(row[x], (28, 14, 16), outline)
                    body = max(0.0, min(1.0, 0.5 - d))
                    if body > 0:
                        row[x] = mix(row[x], col, body)

    def png(self, path):
        def chunk(kind, data):
            body = kind + data
            return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xffffffff)
        compressor = zlib.compressobj(6)
        data = b"".join(compressor.compress(b"\x00" + bytes(round(v) for p in row for v in p))
                        for row in self.pixels) + compressor.flush()
        with open(path, "wb") as out:
            out.write(b"\x89PNG\r\n\x1a\n")
            out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", self.W, self.H, 8, 2, 0, 0, 0)))
            out.write(chunk(b"IDAT", data))
            out.write(chunk(b"IEND", b""))

# ------------------------------------------------------------------

ART = pictures()
MISSING = [{(25, 3), (25, 4), (24, 4), (25, 9)},                       # Mona Lisa
           {(25, 1), (25, 2), (24, 2), (25, 6), (23, 0), (25, 0)}]     # Starry Night
MARCH = ["Y", "B", "N", "G", "K", "C", "N", "O", "B"]

def draw(name):
    layout = LAYOUTS[name]
    width, height = layout["size"]
    canvas = Canvas(width, height)
    for (left, top), picture, missing in zip(layout["paintings"], ("Mona Lisa", "Starry Night"), MISSING):
        canvas.plate(ART[picture], left, top, layout["cell"], missing)
    x0, x1, y, scale = layout["march"]
    for i, char in enumerate(MARCH):
        t = i / (len(MARCH) - 1)
        heading = 180 - 14 * math.cos(t * math.pi * 1.6)
        colour = (60, 54, 66) if char == "K" else PALETTE[char]
        canvas.ant(x0 + (x1 - x0) * t, y + 22 * math.sin(t * math.pi * 1.6), heading, scale, colour, PALETTE[char])
    cx, cy, heading, scale = layout["ant"]
    canvas.ant(cx, cy, heading, scale, PALETTE["R"], PALETTE["Y"])
    art = os.path.join(tempfile.mkdtemp(), "art.png")
    canvas.png(art)
    # The title, in pixels; the lines after the first sit a little right, as
    # the heavy face's A has a wide side bearing.
    k = canvas.k
    title = layout["title"]
    # With four lines, the last is split in two.
    tagline = ["Hundreds of pictures, cleared ant by ant"]
    if len(title["lines"]) == 4:
        tagline = ["Hundreds of pictures,", "cleared ant by ant"]
    texts = [("Ants", "AvenirNext-Heavy", "#FFFFFF"), ("Pixel Puzzle", "AvenirNext-DemiBold", TAN)] + \
            [(line, "AvenirNext-Medium", "#FFFFFFCC") for line in tagline]
    spec = [dict(text=text, font=font, colour=colour, size=size * k, baseline=baseline * k,
                 x=None if title["x"] == "centre" else (title["x"] + (12 if i else 0)) * k)
            for i, ((text, font, colour), (baseline, size)) in enumerate(zip(texts, title["lines"]))]
    spec_path = os.path.join(os.path.dirname(art), "title.json")
    json.dump(spec, open(spec_path, "w"))
    out = os.path.join(root, layout["out"])
    subprocess.run(["swift", os.path.join(here, "header-title.swift"), art, out, spec_path], check=True)
    subprocess.run(["magick", out, "-alpha", "off", out], check=True)   # no alpha, for the App Store
    print("wrote", layout["out"], f"{width}x{height}", flush=True)

for name in sys.argv[1:] or LAYOUTS:
    draw(name)
