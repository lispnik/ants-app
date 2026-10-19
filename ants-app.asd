;;;; ants-app.asd -- ant boxes, a picture made of cubes, and holes to carry
;;;; the cubes down.
;;;;
;;;; The simulator by default; the device too when IOS_SIGNING_IDENTITY says
;;;; how to sign.  ANTS_DISTRIBUTION=1 is an App Store build, for the device
;;;; alone and without get-task-allow -- tools/testflight.sh makes one, and
;;;; ANTS_BUILD is its build number, which every upload must raise.

(defsystem "ants-app"
  :defsystem-depends-on ("asdf-ios-app")
  :class :ios-app-system
  :build-operation "ios-app-op"
  :entry-point "ants:start"
  :description "A puzzle game: deploy coloured ant boxes so their ants can carry away a pixel picture of cubes, without filling every slot with boxes whose cubes are buried."
  :version #.(or (uiop:getenv "ANTS_BUILD") "1")
  :bundle-short-version "1.7.0"
  :serial t
  :depends-on ("objc/uikit")
  :pathname "src"
  :components ((:file "package")
               (:file "model")
               (:file "view"))

  :bundle-identifier "com.burnsidemk.ants"
  :bundle-name "Ants"
  :bundle-executable "ants"
  :bundle-platforms #.(cond ((uiop:getenv "ANTS_DISTRIBUTION") '(:device))
                            ((uiop:getenv "IOS_SIGNING_IDENTITY") '(:simulator :device))
                            (t '(:simulator)))
  :get-task-allow #.(not (uiop:getenv "ANTS_DISTRIBUTION"))
  ;; iPhone only: an iPad app must allow every orientation for multitasking,
  ;; and the board is laid out for portrait.  An iPad runs it as an iPhone app.
  :bundle-device-family (:iphone)
  :bundle-orientations (:portrait)
  :bundle-status-bar-hidden t
  ;; UIStatusBarHidden is honoured only when the view controller is not the
  ;; one deciding, and the root controller here is the shim's, not ours.
  :bundle-info-plist (("UIViewControllerBasedStatusBarAppearance" . :false)
                      ;; Export compliance, answered: the game encrypts nothing.
                      ("ITSAppUsesNonExemptEncryption" . :false))
  ;; 1024x1024 with no alpha; tools/make-icon.py draws it.
  :bundle-icon "res/Ants.xcassets"
  ;; tools/make-sounds.py synthesises these.  In the bundle's root, where
  ;; SpriteKit's playSoundFileNamed: looks.
  :bundle-resources ("res/sounds/tap.wav" "res/sounds/plop.wav" "res/sounds/hint.wav"
                     "res/sounds/won.wav" "res/sounds/lost.wav")
  :bundle-frameworks ("UIKit" "Foundation" "CoreGraphics" "SpriteKit" "AVFoundation")

  :code-signing-identity #.(or (uiop:getenv "IOS_SIGNING_IDENTITY") :automatic)
  :development-team #.(uiop:getenv "IOS_DEVELOPMENT_TEAM")
  :provisioning-profile #.(uiop:getenv "IOS_PROVISIONING_PROFILE"))
