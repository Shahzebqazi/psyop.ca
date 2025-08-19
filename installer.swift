#!/usr/bin/env swift

import Foundation
import Cocoa
import WebKit

// MARK: - Installer for PSYOP Website Setup.hs
// This script checks for required dependencies and runs the Haskell setup

// ANSI color codes for terminal output
struct Colors {
    static let reset = "\u{001B}[0m"
    static let red = "\u{001B}[31m"
    static let green = "\u{001B}[32m"
    static let yellow = "\u{001B}[33m"
    static let blue = "\u{001B}[34m"
    static let magenta = "\u{001B}[35m"
    static let cyan = "\u{001B}[36m"
    static let white = "\u{001B}[37m"
    static let bold = "\u{001B}[1m"
}

// Logging functions with colors
func logInfo(_ message: String) {
    print("\(Colors.blue)[INFO]\(Colors.reset) \(message)")
}

func logSuccess(_ message: String) {
    print("\(Colors.green)[SUCCESS]\(Colors.reset) \(message)")
}

func logWarning(_ message: String) {
    print("\(Colors.yellow)[WARNING]\(Colors.reset) \(message)")
}

func logError(_ message: String) {
    print("\(Colors.red)[ERROR]\(Colors.reset) \(message)")
}

func logBold(_ message: String) {
    print("\(Colors.bold)\(message)\(Colors.reset)")
}

// MARK: - Dependency Checker
class DependencyChecker {
    
    // Check if a command exists in PATH
    func commandExists(_ command: String) -> Bool {
        let task = Process()
        task.launchPath = "/usr/bin/which"
        task.arguments = [command]
        
        let pipe = Pipe()
        task.standardOutput = pipe
        task.standardError = pipe
        
        do {
            try task.run()
            task.waitUntilExit()
            return task.terminationStatus == 0
        } catch {
            return false
        }
    }
    
    // Check if Homebrew is installed
    func isHomebrewInstalled() -> Bool {
        return commandExists("brew")
    }
    
    // Check if Xcode Command Line Tools are installed
    func areXcodeToolsInstalled() -> Bool {
        return commandExists("xcode-select") && commandExists("clang")
    }
    
    // Check if Haskell Stack is installed
    func isHaskellStackInstalled() -> Bool {
        return commandExists("stack")
    }
    
    // Check if Git is installed
    func isGitInstalled() -> Bool {
        return commandExists("git")
    }
    
    // Check if Node.js is installed (for potential frontend tools)
    func isNodeInstalled() -> Bool {
        return commandExists("node")
    }
    
    // Check if Python3 is installed
    func isPythonInstalled() -> Bool {
        return commandExists("python3")
    }
    
    // Check system requirements
    func checkSystemRequirements() -> [String: Bool] {
        logInfo("Checking system requirements...")
        
        let requirements: [String: Bool] = [
            "Xcode Command Line Tools": areXcodeToolsInstalled(),
            "Homebrew": isHomebrewInstalled(),
            "Git": isGitInstalled(),
            "Haskell Stack": isHaskellStackInstalled(),
            "Python3": isPythonInstalled(),
            "Node.js": isNodeInstalled()
        ]
        
        // Display results
        for (requirement, installed) in requirements {
            let status = installed ? "✅" : "❌"
            let message = installed ? "Installed" : "Not installed"
            print("\(status) \(requirement): \(message)")
        }
        
        return requirements
    }
}

// MARK: - Dependency Installer
class DependencyInstaller {
    
    // Install Xcode Command Line Tools
    func installXcodeTools() -> Bool {
        logInfo("Installing Xcode Command Line Tools...")
        
        if !runCommand("xcode-select --install") {
            logError("Failed to install Xcode Command Line Tools")
            logWarning("Please install manually from: https://developer.apple.com/xcode/")
            return false
        }
        
        logSuccess("Xcode Command Line Tools installation initiated")
        logWarning("Please complete the installation in the popup window and restart this script")
        return false // Return false to indicate manual intervention needed
    }
    
    // Install Homebrew
    func installHomebrew() -> Bool {
        logInfo("Installing Homebrew...")
        
        let installScript = "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
        
        if !runCommand(installScript) {
            logError("Failed to install Homebrew")
            return false
        }
        
        // Add Homebrew to PATH if needed
        let shellConfig = getShellConfigFile()
        let homebrewPath = "export PATH=\"/opt/homebrew/bin:$PATH\""
        
        if !runCommand("echo '\(homebrewPath)' >> \(shellConfig)") {
            logWarning("Failed to add Homebrew to PATH. Please add manually to \(shellConfig)")
        }
        
        logSuccess("Homebrew installed successfully")
        return true
    }
    
    // Install Haskell Stack
    func installHaskellStack() -> Bool {
        logInfo("Installing Haskell Stack...")
        
        if !runCommand("curl -sSL https://get.haskellstack.org/ | sh") {
            logError("Failed to install Haskell Stack")
            return false
        }
        
        logSuccess("Haskell Stack installed successfully")
        return true
    }
    
    // Install Git
    func installGit() -> Bool {
        logInfo("Installing Git...")
        
        if !runCommand("brew install git") {
            logError("Failed to install Git")
            return false
        }
        
        logSuccess("Git installed successfully")
        return true
    }
    
    // Install Python3
    func installPython3() -> Bool {
        logInfo("Installing Python3...")
        
        if !runCommand("brew install python") {
            logError("Failed to install Python3")
            return false
        }
        
        logSuccess("Python3 installed successfully")
        return true
    }
    
    // Install Node.js
    func installNode() -> Bool {
        logInfo("Installing Node.js...")
        
        if !runCommand("brew install node") {
            logError("Failed to install Node.js")
            return false
        }
        
        logSuccess("Node.js installed successfully")
        return true
    }
    
    // Get the shell configuration file
    func getShellConfigFile() -> String {
        let shell = ProcessInfo.processInfo.environment["SHELL"] ?? "/bin/zsh"
        
        if shell.contains("zsh") {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.zshrc"
        } else if shell.contains("bash") {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.bash_profile"
        } else {
            return ProcessInfo.processInfo.environment["HOME"]! + "/.profile"
        }
    }
    
    // Install all missing dependencies
    func installMissingDependencies(_ requirements: [String: Bool]) -> Bool {
        logInfo("Installing missing dependencies...")
        
        var allInstalled = true
        
        // Install Xcode Tools first if needed
        if !requirements["Xcode Command Line Tools"]! {
            if !installXcodeTools() {
                return false
            }
        }
        
        // Install Homebrew if needed
        if !requirements["Homebrew"]! {
            if !installHomebrew() {
                allInstalled = false
            }
        }
        
        // Install other dependencies using Homebrew
        if requirements["Homebrew"]! {
            if !requirements["Git"]! {
                if !installGit() {
                    allInstalled = false
                }
            }
            
            if !requirements["Python3"]! {
                if !installPython3() {
                    allInstalled = false
                }
            }
            
            if !requirements["Node.js"]! {
                if !installNode() {
                    allInstalled = false
                }
            }
        }
        
        // Install Haskell Stack
        if !requirements["Haskell Stack"]! {
            if !installHaskellStack() {
                allInstalled = false
            }
        }
        
        return allInstalled
    }
}

// MARK: - Setup Runner
class SetupRunner {
    
    // Check if dev-server.hs exists
    func setupFileExists() -> Bool {
        let fileManager = FileManager.default
        return fileManager.fileExists(atPath: "config/dev-server.hs")
    }
    
    // Check if we're in the right directory
    func checkProjectStructure() -> Bool {
        let fileManager = FileManager.default
        let requiredFiles = ["src", "package.yaml", "stack.yaml"]
        
        for file in requiredFiles {
            if !fileManager.fileExists(atPath: file) {
                logError("Required file/directory not found: \(file)")
                return false
            }
        }
        
        return true
    }
    
    // Run the dev-server.hs setup
    func runSetup() -> Bool {
        logInfo("Running development server setup...")
        
        if !setupFileExists() {
            logError("config/dev-server.hs not found in current directory")
            return false
        }
        
        if !checkProjectStructure() {
            logError("Project structure is incomplete")
            return false
        }
        
        // Try to run setup with runhaskell first
        logInfo("Attempting to run setup with runhaskell...")
        if runCommand("runhaskell config/dev-server.hs setup") {
            logSuccess("Development server setup completed successfully")
            return true
        }
        
        // Fallback to stack exec
        logWarning("runhaskell failed, trying with stack...")
        if runCommand("stack exec runhaskell config/dev-server.hs setup") {
            logSuccess("Development server setup completed successfully with stack")
            return true
        }
        
        // Final fallback to ghc
        logWarning("stack failed, trying with ghc...")
        if runCommand("ghc config/dev-server.hs && ./dev-server setup") {
            logSuccess("Development server setup completed successfully with ghc")
            return true
        }
        
        logError("All methods to run dev-server setup failed")
        logInfo("Let's try to diagnose the issue...")
        
        // Diagnostic information
        let (stackVersion, stackOutput) = runCommandWithOutput("stack --version")
        if stackVersion {
            logInfo("Stack version: \(stackOutput)")
        } else {
            logWarning("Stack version check failed: \(stackOutput)")
        }
        
        let (ghcVersion, ghcOutput) = runCommandWithOutput("ghc --version")
        if ghcVersion {
            logInfo("GHC version: \(ghcOutput)")
        } else {
            logWarning("GHC version check failed: \(ghcOutput)")
        }
        
        let (runhaskellVersion, runhaskellOutput) = runCommandWithOutput("runhaskell --version")
        if runhaskellVersion {
            logInfo("runhaskell version: \(runhaskellOutput)")
        } else {
            logWarning("runhaskell version check failed: \(runhaskellOutput)")
        }
        
        return false
    }
}

// MARK: - Utility Functions
func runCommand(_ command: String) -> Bool {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c", command]
    
    let pipe = Pipe()
    let errorPipe = Pipe()
    task.standardOutput = pipe
    task.standardError = errorPipe
    
    do {
        try task.run()
        task.waitUntilExit()
        
        let outputData = pipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
        
        let output = String(data: outputData, encoding: .utf8) ?? ""
        let error = String(data: errorData, encoding: .utf8) ?? ""
        
        if task.terminationStatus != 0 {
            logWarning("Command failed: \(command)")
            if !output.isEmpty { print("Output: \(output)") }
            if !error.isEmpty { print("Error: \(error)") }
            return false
        }
        
        if !output.isEmpty { print("Output: \(output)") }
        return true
    } catch {
        logError("Failed to run command: \(command)")
        logError("Error: \(error)")
        return false
    }
}

func runCommandWithOutput(_ command: String) -> (success: Bool, output: String) {
    let task = Process()
    task.launchPath = "/bin/bash"
    task.arguments = ["-c", command]
    
    let pipe = Pipe()
    let errorPipe = Pipe()
    task.standardOutput = pipe
    task.standardError = errorPipe
    
    do {
        try task.run()
        task.waitUntilExit()
        
        let outputData = pipe.fileHandleForReading.readDataToEndOfFile()
        let errorData = errorPipe.fileHandleForReading.readDataToEndOfFile()
        
        let output = String(data: outputData, encoding: .utf8) ?? ""
        let error = String(data: errorData, encoding: .utf8) ?? ""
        
        let fullOutput = output + (error.isEmpty ? "" : "\nError: \(error)")
        
        return (task.terminationStatus == 0, fullOutput)
    } catch {
        logError("Failed to run command: \(command)")
        logError("Error: \(error)")
        return (false, "")
    }
}

// MARK: - GUI Testing System
// This section provides GUI testing capabilities for the PSYOP website

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
            let window = PSYOPWindow(
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
class PSYOPWindow: NSWindow {
    private let webView: WKWebView
    private let index: Int
    
    init(width: Int, height: Int, title: String, index: Int) {
        self.index = index
        
        // Create WebView
        let webViewConfiguration = WKWebViewConfiguration()
        // Note: Some properties may not be available in all macOS versions
        // The WebView will work with default configuration
        
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
extension PSYOPWindow: NSWindowDelegate {
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

// MARK: - GUI Application Delegate
class PSYOPGUIAppDelegate: NSObject, NSApplicationDelegate {
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

// MARK: - GUI Launch Function
func launchGUI() {
    logInfo("Launching PSYOP Website GUI Test...")
    
    let app = NSApplication.shared
    let delegate = PSYOPGUIAppDelegate()
    app.delegate = delegate
    app.run()
}

// MARK: - Enhanced Main Function
func main() {
    let args = CommandLine.arguments
    
    if args.count > 1 {
        let command = args[1]
        
        switch command {
        case "gui", "test-ui":
            logInfo("Starting GUI test mode...")
            launchGUI()
        case "check":
            logInfo("Checking system requirements only...")
            let checker = DependencyChecker()
            _ = checker.checkSystemRequirements()
        case "install-deps":
            logInfo("Installing dependencies only...")
            let checker = DependencyChecker()
            let installer = DependencyInstaller()
            let requirements = checker.checkSystemRequirements()
            _ = installer.installMissingDependencies(requirements)
        case "run-setup":
            logInfo("Running setup only...")
            let runner = SetupRunner()
            _ = runner.runSetup()
        case "full":
            logInfo("Running full installation...")
            let checker = DependencyChecker()
            let installer = DependencyInstaller()
            let requirements = checker.checkSystemRequirements()
            _ = installer.installMissingDependencies(requirements)
            let runner = SetupRunner()
            _ = runner.runSetup()
        default:
            logError("Unknown command: \(command)")
            print("Usage: swift installer.swift [COMMAND]")
            print("")
            print("Commands:")
            print("  gui, test-ui  Launch GUI test with 4 responsive windows")
            print("  check        Check system requirements")
            print("  install-deps Install missing dependencies")
            print("  run-setup    Run Setup.hs only")
            print("  full         Run full installation (check, install, run setup)")
            print("")
        }
    } else {
        logInfo("No command specified, running full installation...")
        let checker = DependencyChecker()
        let installer = DependencyInstaller()
        let requirements = checker.checkSystemRequirements()
        _ = installer.installMissingDependencies(requirements)
        let runner = SetupRunner()
        _ = runner.runSetup()
    }
}

// Run the main function
main()
