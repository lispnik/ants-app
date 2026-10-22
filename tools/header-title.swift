// header-title.swift -- set the header's title over docs/header-art.png,
// with the system's Avenir Next, which ImageMagick cannot open by face.
//
//     swift tools/header-title.swift docs/header-art.png docs/header.png
import AppKit

let args = CommandLine.arguments
guard args.count == 3, let art = NSImage(contentsOfFile: args[1]),
      let rep = art.representations.first else {
    FileHandle.standardError.write("usage: header-title.swift in.png out.png\n".data(using: .utf8)!)
    exit(1)
}
let width = rep.pixelsWide, height = rep.pixelsHigh
let canvas = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: width, pixelsHigh: height,
                              bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                              colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: canvas)
art.draw(in: NSRect(x: 0, y: 0, width: width, height: height))

// Lines from the top: text, font, size, colour, x, baseline from the top.
let shadow = NSShadow()
shadow.shadowColor = NSColor(white: 0, alpha: 0.35)
shadow.shadowOffset = NSSize(width: 6, height: -8)
shadow.shadowBlurRadius = 14
let lines: [(String, String, CGFloat, NSColor, CGFloat, CGFloat)] = [
    ("Ants", "AvenirNext-Heavy", 260, .white, 112, 390),
    ("Pixel Puzzle", "AvenirNext-DemiBold", 86, NSColor(red: 0.95, green: 0.84, blue: 0.66, alpha: 1), 124, 500),
    ("93 pictures, cleared ant by ant", "AvenirNext-Medium", 42, NSColor(white: 1, alpha: 0.8), 128, 585),
]
for (text, fontName, size, colour, x, baseline) in lines {
    let font = NSFont(name: fontName, size: size)!
    let attributes: [NSAttributedString.Key: Any] = [.font: font, .foregroundColor: colour, .shadow: shadow]
    NSAttributedString(string: text, attributes: attributes)
        .draw(at: NSPoint(x: x, y: CGFloat(height) - baseline + font.descender))
}
NSGraphicsContext.restoreGraphicsState()
try! canvas.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: args[2]))
