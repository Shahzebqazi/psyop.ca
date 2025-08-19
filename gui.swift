#!/usr/bin/env swift

import Cocoa
import WebKit

// MARK: - Window Manager
class PSYOPWindowManager: NSObject {
    static let shared = PSYOPWindowManager()
    
    private var windows: [PSYOPWindow] = []
    private let windowSizes = [
        (600, 400),      // Small landscape
        (1280, 720),     // Standard HD
        (800, 1600),     // Tall portrait
        (1920, 1080)     // Full HD
    ]
    
    private override init() {
        super.init()
        setupWindows()
    }
    
    private func setupWindows() {
        for (index, size) in windowSizes.enumerated() {
            let window = PSOPWindow(
                width: size.0,
                height: size.1,
                title: "PSYOP Website Test - \(size.0)×\(size.1)",
                index: index
            )
            windows.append(window)
        }
    }
    
    func showAllWindows() {
        for window in windows {
            window.makeKeyAndOrderFront(nil)
        }
    }
    
    func hideAllWindows() {
        for window in windows {
            window.orderOut(nil)
        }
    }
}

// MARK: - PSYOP Window
class PSOPWindow: NSWindow {
    private let webView: WKWebView
    private let index: Int
    
    init(width: Int, height: Int, title: String, index: Int) {
        self.index = index
        
        // Create WebView
        let webViewConfiguration = WKWebViewConfiguration()
        webViewConfiguration.allowsInlineMediaPlayback = true
        webViewConfiguration.mediaTypesRequiringUserActionForPlayback = []
        
        self.webView = WKWebView(frame: NSRect(x: 0, y: 0, width: width, height: height), configuration: webViewConfiguration)
        
        // Window configuration
        let windowRect = NSRect(
            x: 100 + (index * 50),  // Stagger windows
            y: 100 + (index * 50),
            width: width,
            height: height
        )
        
        super.init(
            contentRect: windowRect,
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        
        self.title = title
        self.delegate = self
        
        // Set content view
        self.contentView = webView
        
        // Load PSYOP website
        loadPSYOPWebsite()
        
        // Restore window position and size
        restoreWindowState()
    }
    
    private func loadPSYOPWebsite() {
        // Try to load from local development server first
        if let localURL = URL(string: "http://localhost:8080") {
            let request = URLRequest(url: localURL)
            webView.load(request)
            
            // Fallback to local file if server is not running
            DispatchQueue.main.asyncAfter(deadline: .now() + 2.0) {
                if self.webView.url == nil {
                    self.loadLocalFallback()
                }
            }
        } else {
            loadLocalFallback()
        }
    }
    
    private func loadLocalFallback() {
        if let localFile = Bundle.main.path(forResource: "public/index", ofType: "html") {
            let url = URL(fileURLWithPath: localFile)
            webView.loadFileURL(url, allowingReadAccessTo: url.deletingLastPathComponent())
        } else {
            // Load a simple test page
            let html = """
            <!DOCTYPE html>
            <html>
            <head>
                <title>PSYOP Website Test - \(self.title)</title>
                <style>
                    body { 
                        margin: 0; 
                        padding: 20px; 
                        font-family: -apple-system, BlinkMacSystemFont, sans-serif;
                        background: linear-gradient(45deg, #1a1a1a, #2a2a2a);
                        color: white;
                        min-height: 100vh;
                    }
                    .container { text-align: center; padding-top: 100px; }
                    h1 { color: #ff6b6b; font-size: 2.5em; margin-bottom: 20px; }
                    p { font-size: 1.2em; margin-bottom: 15px; }
                    .status { 
                        background: rgba(255, 255, 255, 0.1); 
                        padding: 20px; 
                        border-radius: 10px; 
                        margin: 20px 0;
                    }
                    .button {
                        background: #ff6b6b;
                        color: white;
                        border: none;
                        padding: 12px 24px;
                        border-radius: 6px;
                        font-size: 1.1em;
                        cursor: pointer;
                        margin: 10px;
                    }
                    .button:hover { background: #ff5252; }
                </style>
            </head>
            <body>
                <div class="container">
                    <h1>🎵 PSYOP Website Test</h1>
                    <p>Window Size: \(self.frame.width) × \(self.frame.height)</p>
                    <p>This is a test window for UX/UI development</p>
                    
                    <div class="status">
                        <h3>Window Information</h3>
                        <p><strong>Index:</strong> \(self.index)</p>
                        <p><strong>Title:</strong> \(self.title)</p>
                        <p><strong>Frame:</strong> \(Int(self.frame.origin.x)), \(Int(self.frame.origin.y))</p>
                    </div>
                    
                    <button class="button" onclick="testScroll()">Test Scrolling</button>
                    <button class="button" onclick="testAnimation()">Test Animation</button>
                    <button class="button" onclick="testResponsive()">Test Responsive</button>
                </div>
                
                <script>
                    function testScroll() {
                        window.scrollTo(0, 1000);
                        setTimeout(() => window.scrollTo(0, 0), 1000);
                    }
                    
                    function testAnimation() {
                        document.body.style.background = 'linear-gradient(45deg, #ff6b6b, #4ecdc4)';
                        setTimeout(() => {
                            document.body.style.background = 'linear-gradient(45deg, #1a1a1a, #2a2a2a)';
                        }, 1000);
                    }
                    
                    function testResponsive() {
                        const width = window.innerWidth;
                        const height = window.innerHeight;
                        alert('Responsive Test:\\nWidth: ' + width + '\\nHeight: ' + height);
                    }
                    
                    // Add some dynamic content
                    setInterval(() => {
                        const time = new Date().toLocaleTimeString();
                        document.title = 'PSYOP Test - ' + time;
                    }, 1000);
                </script>
            </body>
            </html>
            """
            
            webView.loadHTMLString(html, baseURL: nil)
        }
    }
    
    private func saveWindowState() {
        let defaults = UserDefaults.standard
        let key = "PSYOPWindow_\(index)"
        
        let state: [String: Any] = [
            "frame": NSStringFromRect(frame),
            "isVisible": isVisible
        ]
        
        defaults.set(state, forKey: key)
    }
    
    private func restoreWindowState() {
        let defaults = UserDefaults.standard
        let key = "PSYOPWindow_\(index)"
        
        if let state = defaults.dictionary(forKey: key),
           let frameString = state["frame"] as? String {
            let frame = NSRectFromString(frameString)
            setFrame(frame, display: true)
        }
    }
}

// MARK: - Window Delegate
extension PSOPWindow: NSWindowDelegate {
    func windowDidResize(_ notification: Notification) {
        // Update web view size
        webView.frame = contentView?.bounds ?? NSRect.zero
        
        // Save new size
        saveWindowState()
    }
    
    func windowDidMove(_ notification: Notification) {
        // Save new position
        saveWindowState()
    }
    
    func windowWillClose(_ notification: Notification) {
        // Save final state
        saveWindowState()
    }
}

// MARK: - Main Application
class PSYOPAppDelegate: NSObject, NSApplicationDelegate {
    private var windowManager: PSYOPWindowManager!
    
    func applicationDidFinishLaunching(_ notification: Notification) {
        // Create window manager
        windowManager = PSYOPWindowManager.shared
        
        // Show all windows
        windowManager.showAllWindows()
        
        // Set up menu
        setupMenu()
        
        // Set activation policy
        NSApp.setActivationPolicy(.regular)
        NSApp.activate(ignoringOtherApps: true)
    }
    
    private func setupMenu() {
        let mainMenu = NSMenu()
        
        // App menu
        let appMenuItem = NSMenuItem()
        let appMenu = NSMenu()
        appMenuItem.submenu = appMenu
        
        appMenu.addItem(NSMenuItem(title: "About PSYOP Test", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: ""))
        appMenu.addItem(NSMenuItem.separator())
        appMenu.addItem(NSMenuItem(title: "Quit", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q"))
        
        mainMenu.addItem(appMenuItem)
        
        // Window menu
        let windowMenuItem = NSMenuItem()
        let windowMenu = NSMenu()
        windowMenuItem.submenu = windowMenu
        
        windowMenu.addItem(NSMenuItem(title: "Show All Windows", action: #selector(showAllWindows), keyEquivalent: "1"))
        windowMenu.addItem(NSMenuItem(title: "Hide All Windows", action: #selector(hideAllWindows), keyEquivalent: "2"))
        windowMenu.addItem(NSMenuItem.separator())
        windowMenu.addItem(NSMenuItem(title: "Minimize", action: #selector(NSWindow.miniaturize(_:)), keyEquivalent: "m"))
        windowMenu.addItem(NSMenuItem(title: "Zoom", action: #selector(NSWindow.zoom(_:)), keyEquivalent: ""))
        windowMenu.addItem(NSMenuItem.separator())
        windowMenu.addItem(NSMenuItem(title: "Bring All to Front", action: #selector(NSApplication.arrangeInFront(_:)), keyEquivalent: ""))
        
        mainMenu.addItem(windowMenuItem)
        
        // Help menu
        let helpMenuItem = NSMenuItem()
        let helpMenu = NSMenu()
        helpMenuItem.submenu = helpMenu
        
        helpMenu.addItem(NSMenuItem(title: "PSYOP Website Help", action: #selector(showHelp), keyEquivalent: "?"))
        
        mainMenu.addItem(helpMenuItem)
        
        NSApp.mainMenu = mainMenu
    }
    
    @objc private func showAllWindows() {
        windowManager.showAllWindows()
    }
    
    @objc private func hideAllWindows() {
        windowManager.hideAllWindows()
    }
    
    @objc private func showHelp() {
        let alert = NSAlert()
        alert.messageText = "PSYOP Website Test Application"
        alert.informativeText = """
        This application opens 4 test windows to test the PSYOP website UX/UI:
        
        • 600×400 - Small landscape window
        • 1280×720 - Standard HD window  
        • 800×1600 - Tall portrait window
        • 1920×1080 - Full HD window
        
        Each window:
        • Remembers its size and position
        • Supports responsive resizing
        • Tests scrolling and animations
        • Loads the PSYOP website from localhost:8080
        
        Use the Window menu to show/hide all windows.
        """
        alert.alertStyle = .informational
        alert.addButton(withTitle: "OK")
        alert.runModal()
    }
    
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        return true
    }
}

// MARK: - Main Entry Point
let app = NSApplication.shared
let delegate = PSYOPAppDelegate()
app.delegate = delegate
app.run()
