// caption-screenshots.swift -- set each App Store screenshot under a caption:
// a headline and a line under it, in Avenir Next, above the screenshot made
// smaller, in a dark bezel with its corners rounded like a phone's.  The
// result is the screenshot's own size, as App Store Connect wants.
//
//     swift tools/caption-screenshots.swift IN-DIR OUT-DIR
//
// Captions are keyed by the screenshot's file name, below.
import AppKit

let captions: [String: (String, String)] = [
    "1-mona-lisa.png": ("Clear the masterpiece", "Ants carry it off, cube by cube"),
    "2-the-scream.png": ("Plan every tap", "Send the right ants at the right time"),
    "3-cleared.png": ("Beat the par", "Earn up to three stars a picture"),
    "4-pictures.png": ("113 pictures", "From a mushroom to the Mona Lisa"),
    "5-settings.png": ("Play your way", "Turn off the clock, sound or buzz"),
]

let args = CommandLine.arguments
guard args.count == 3 else {
    FileHandle.standardError.write("usage: caption-screenshots.swift IN-DIR OUT-DIR\n".data(using: .utf8)!)
    exit(1)
}
let fm = FileManager.default
try? fm.createDirectory(atPath: args[2], withIntermediateDirectories: true)

func rgb(_ r: Int, _ g: Int, _ b: Int, _ a: CGFloat = 1) -> NSColor {
    NSColor(srgbRed: CGFloat(r) / 255, green: CGFloat(g) / 255, blue: CGFloat(b) / 255, alpha: a)
}

for name in captions.keys.sorted() {
    let (headline, subline) = captions[name]!
    let path = (args[1] as NSString).appendingPathComponent(name)
    guard let shot = NSImage(contentsOfFile: path), let rep = shot.representations.first else {
        print("missing \(path)"); continue
    }
    let w = CGFloat(rep.pixelsWide), h = CGFloat(rep.pixelsHigh)
    let canvas = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: rep.pixelsWide, pixelsHigh: rep.pixelsHigh,
                                  bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                                  colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
    canvas.size = NSSize(width: w, height: h)               // one point a pixel
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: canvas)

    // The game's green, deepening downwards.
    NSGradient(starting: rgb(84, 110, 70), ending: rgb(34, 45, 29))!
        .draw(in: NSRect(x: 0, y: 0, width: w, height: h), angle: -90)

    // The caption, centred, in the top fifth.
    func line(_ text: String, _ font: String, _ size: CGFloat, _ colour: NSColor, baseline: CGFloat) {
        let f = NSFont(name: font, size: size)!
        let shadow = NSShadow()
        shadow.shadowColor = NSColor(white: 0, alpha: 0.35)
        shadow.shadowOffset = NSSize(width: 0, height: -size * 0.04)
        shadow.shadowBlurRadius = size * 0.08
        var s = NSAttributedString(string: text, attributes: [.font: f, .foregroundColor: colour, .shadow: shadow])
        if s.size().width > w * 0.9 {                       // shrink to fit, never wrap
            let fitted = NSFont(name: font, size: size * w * 0.9 / s.size().width)!
            s = NSAttributedString(string: text, attributes: [.font: fitted, .foregroundColor: colour, .shadow: shadow])
        }
        s.draw(at: NSPoint(x: (w - s.size().width) / 2, y: h - baseline + f.descender))
    }
    line(headline, "AvenirNext-Bold", w * 0.082, rgb(255, 255, 255), baseline: h * 0.085)
    line(subline, "AvenirNext-DemiBold", w * 0.045, rgb(242, 214, 168), baseline: h * 0.130)

    // The screenshot, smaller, in a dark bezel with round corners.
    let sw = w * 0.80, sh = h * 0.80, top = h * 0.165
    let bezel = w * 0.022
    let screen = NSRect(x: (w - sw) / 2, y: h - top - sh, width: sw, height: sh)
    let frame = screen.insetBy(dx: -bezel, dy: -bezel)
    NSGraphicsContext.saveGraphicsState()
    let drop = NSShadow()
    drop.shadowColor = NSColor(white: 0, alpha: 0.45)
    drop.shadowOffset = NSSize(width: 0, height: -w * 0.012)
    drop.shadowBlurRadius = w * 0.035
    drop.set()
    rgb(18, 18, 20).setFill()
    NSBezierPath(roundedRect: frame, xRadius: w * 0.105, yRadius: w * 0.105).fill()
    NSGraphicsContext.restoreGraphicsState()
    NSGraphicsContext.saveGraphicsState()
    NSBezierPath(roundedRect: screen, xRadius: w * 0.085, yRadius: w * 0.085).addClip()
    shot.draw(in: screen)
    NSGraphicsContext.restoreGraphicsState()

    NSGraphicsContext.restoreGraphicsState()
    // App Store Connect refuses an alpha channel; flatten to RGB.
    let cg = CGContext(data: nil, width: rep.pixelsWide, height: rep.pixelsHigh, bitsPerComponent: 8,
                       bytesPerRow: 0, space: CGColorSpace(name: CGColorSpace.sRGB)!,
                       bitmapInfo: CGImageAlphaInfo.noneSkipLast.rawValue)!
    cg.draw(canvas.cgImage!, in: CGRect(x: 0, y: 0, width: w, height: h))
    let flat = NSBitmapImageRep(cgImage: cg.makeImage()!)
    let out = (args[2] as NSString).appendingPathComponent(name)
    try! flat.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: out))
    print("wrote \(out)")
}
