#!/usr/bin/env python3
"""make-icon.py -- write res/Ants.xcassets, the app icon, with the standard
library alone: the Mushroom level's picture as cubes on the plate, one cube
gone, and the ant that took it.  1024x1024, no alpha, as iOS requires.

    python3 tools/make-icon.py
"""
import json, os, struct, zlib

SIZE = 1024
PICTURE = ["....RRRR....",
           "..RRWWRRRR..",
           ".RRWWWWRRRR.",
           ".RRRWWRRWWR.",
           "RRRRRRRWWWWR",
           "RWWRRRRRWWRR",
           "RWWWRRRRRRRR",
           ".RRRRRRRRRR.",
           "...TTTTTT...",
           "...TKTTKT...",
           "...TTTTTT...",
           "....TTT....."]           # the cube the ant has, missing
PALETTE = {"R": (232, 61, 56), "W": (247, 247, 242), "T": (242, 214, 168), "K": (51, 46, 56)}
GROUND = (66, 87, 56)
PLATE = (204, 199, 189)

pixels = [[GROUND] * SIZE for _ in range(SIZE)]

def fill(x0, y0, x1, y1, colour):
    for y in range(max(0, int(y0)), min(SIZE, int(y1))):
        row = pixels[y]
        for x in range(max(0, int(x0)), min(SIZE, int(x1))):
            row[x] = colour

def shade(colour, k):
    return tuple(max(0, min(255, int(c * k))) for c in colour)

def cube(x, y, size, colour):
    fill(x, y, x + size, y + size, colour)
    fill(x, y, x + size, y + size * 0.22, shade(colour, 1.18) if colour != PALETTE["W"] else (255, 255, 255))
    fill(x, y + size * 0.84, x + size, y + size, shade(colour, 0.8))

cell = 64
left = (SIZE - 12 * cell) // 2
top = 112
fill(left - 28, top - 28, left + 12 * cell + 28, top + 12 * cell + 28, PLATE)
for r, row in enumerate(PICTURE):
    for c, char in enumerate(row):
        if char != ".":
            cube(left + c * cell + 3, top + r * cell + 3, cell - 6, PALETTE[char])

# The ant, below the plate, walking right with its tan cube.
ax, ay = 600, 944
dark = (30, 24, 24)
for dy in (-16, 16):                                  # legs
    fill(ax - 70, ay + dy - 5, ax + 40, ay + dy + 5, dark)
fill(ax - 90, ay - 22, ax - 30, ay + 22, shade(PALETTE["R"], 0.9))   # abdomen
fill(ax - 30, ay - 16, ax + 10, ay + 16, shade(PALETTE["R"], 0.75))  # thorax
fill(ax + 10, ay - 18, ax + 46, ay + 18, shade(PALETTE["R"], 0.55))  # head
cube(ax + 50, ay - 40, 64, PALETTE["T"])

def png(path):
    raw = b"".join(b"\x00" + bytes(v for p in row for v in p) for row in pixels)
    def chunk(kind, data):
        body = kind + data
        return struct.pack(">I", len(data)) + body + struct.pack(">I", zlib.crc32(body) & 0xffffffff)
    with open(path, "wb") as out:
        out.write(b"\x89PNG\r\n\x1a\n")
        out.write(chunk(b"IHDR", struct.pack(">IIBBBBB", SIZE, SIZE, 8, 2, 0, 0, 0)))  # RGB, no alpha
        out.write(chunk(b"IDAT", zlib.compress(raw, 9)))
        out.write(chunk(b"IEND", b""))

here = os.path.dirname(os.path.abspath(__file__))
catalogue = os.path.join(here, "..", "res", "Ants.xcassets")
iconset = os.path.join(catalogue, "AppIcon.appiconset")
os.makedirs(iconset, exist_ok=True)
info = {"author": "ants-app", "version": 1}
json.dump({"info": info}, open(os.path.join(catalogue, "Contents.json"), "w"), indent=2)
json.dump({"images": [{"filename": "icon.png", "idiom": "universal", "platform": "ios", "size": "1024x1024"}],
           "info": info}, open(os.path.join(iconset, "Contents.json"), "w"), indent=2)
png(os.path.join(iconset, "icon.png"))
print("wrote", os.path.normpath(iconset))
